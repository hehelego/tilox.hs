{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module NaiveEval
  ( ValType (..),
    Val (..),
    Env (..),
    VMstate (..),
    runProg,
    runState,
    emptyEnv,
  )
where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.Maybe (fromJust, fromMaybe, isJust)
import qualified Parser as P

data ValType = NilT | BoolT | NumberT | StringT deriving (Show)

data Val = Nil | Bool Bool | Number Double | String String deriving (Eq)

type Assignment = [(P.Ident, Val)]

assgnHas :: P.Ident -> Assignment -> Bool
assgnHas id assgn = isJust $ lookup id assgn

assgnUpd :: P.Ident -> Val -> Assignment -> Assignment
assgnUpd id nv = map $ \(x, v) -> (x, if x == id then nv else v)

data Env = Env
  { assgn :: Assignment,
    parent :: Maybe Env
  }

emptyEnv = Env [] Nothing

envLookup :: P.Ident -> Env -> Maybe Val
envLookup id env = if isJust cur then cur else par
  where
    cur = lookup id $ assgn env
    par = parent env >>= envLookup id

envModify :: P.Ident -> Val -> Env -> Env
envModify id v env@(Env assgn parent) =
  if assgnHas id assgn
    then env {assgn = assgnUpd id v assgn}
    else env {parent = envModify id v <$> parent}

envAdd :: P.Ident -> Val -> Env -> Env
envAdd id val env = env {assgn = (id, val) : assgn env}

envPush :: Env -> Env
envPush env = Env {assgn = [], parent = Just env}

envPop :: Env -> Maybe Env
envPop = parent

data Err = TypeMismatch {expectedT :: ValType, actualT :: ValType} | NotInScope {referred :: P.Ident} | AlreadyDeclared {redef :: P.Ident}

instance Show Err where
  show (TypeMismatch exp act) = "ERROR: type miss match," ++ "expected " ++ show exp ++ "actual " ++ show act
  show (NotInScope ref) = "ERROR: " ++ show ref ++ " not in scope"
  show (AlreadyDeclared ref) = "ERROR: " ++ show ref ++ " is already defined"
  show _ = error "unknown error"

type VMstate a = State Env Err a

runProg :: P.Prog -> VMstate Val
runProg (P.Prog decls) = foldl (\s d -> s >> runDecl d) (pure Nil) decls

runDecl :: P.Decl -> VMstate Val
runDecl (P.StmtDecl stmt) = runStmt stmt
runDecl (P.VarDecl var init) = runVarDecl var init

runStmt :: P.Stmt -> VMstate Val
runStmt (P.ExprStmt e) = eval e

runVarDecl :: P.Ident -> Maybe P.Expr -> VMstate Val
runVarDecl id init =
  do
    env <- get
    case envLookup id env of
      Just _ -> raise $ AlreadyDeclared id
      Nothing -> pure ()
    iv <- maybe (pure Nil) eval init
    modify $ envAdd id iv
    pure Nil

instance Show Val where
  show Nil = "nil"
  show (Bool b) = if b then "true" else "false"
  show (Number n) = show n
  show (String s) = show s
  show _ = error "unsupported value"

unwrapBool :: Val -> Bool
unwrapBool (Bool b) = b
unwrapBool _ = error "not a boolean value"

unwrapNum :: Val -> Double
unwrapNum (Number n) = n
unwrapNum _ = error "not a number value"

unwrapString :: Val -> String
unwrapString (String s) = s
unwrapString _ = error "not a string value"

unwrapNil :: Val -> ()
unwrapNil Nil = ()
unwrapNil _ = error "not a nil value"

eval :: P.Expr -> VMstate Val
eval (P.LiteralExpr lit) = evalLiteral lit
eval (P.UnaryExpr uop sub) = evalUnary uop sub
eval (P.BinaryExpr bop lhs rhs) = evalBinary bop lhs rhs
eval (P.PrintExpr e) = evalPrint e
eval (P.AssignExpr id e) = evalAssign id e
eval _ = error "unsupported expression"

evalLiteral :: P.Literal -> VMstate Val
evalLiteral (P.Ref ident) = do
  env <- get
  case envLookup ident env of
    Just v -> pure v
    Nothing -> raise $ NotInScope ident
evalLiteral (P.Number n) = pure $ Number n
evalLiteral (P.String s) = pure $ String s
evalLiteral (P.Bool b) = pure $ Bool b
evalLiteral P.Nil = pure Nil
evalLiteral _ = error "unsupported literal"

evalUnary :: P.UnaryOp -> P.Expr -> VMstate Val
evalUnary op sub = let v = eval sub in opfunc op <$> v
  where
    opfunc op = case op of
      P.Neg -> negf
      P.Not -> notf
      _ -> error "unsupported unary operator"
    negf v = Number (-unwrapNum v)
    notf v = Bool (not $ unwrapBool v)

evalBinary :: P.BinaryOp -> P.Expr -> P.Expr -> VMstate Val
evalBinary op lhs rhs =
  let lv = eval lhs; rv = eval rhs
   in opfunc op <$> lv <*> rv
  where
    opfunc op = case op of
      P.Eq -> eq
      P.Neq -> neq
      P.Lt -> lt
      P.Leq -> leq
      P.Gt -> gt
      P.Geq -> geq
      P.Plus -> plus
      P.Minus -> minus
      P.Times -> times
      P.Divides -> divides
      _ -> error "unknown binary operator"
    makeop lift unwrap op l r = lift $ unwrap l `op` unwrap r
    eq = makeop Bool id (==)
    neq = makeop Bool id (/=)
    lt = makeop Bool unwrapNum (<)
    leq = makeop Bool unwrapNum (<=)
    gt = makeop Bool unwrapNum (<=)
    geq = makeop Bool unwrapNum (<=)
    plus = makeop Number unwrapNum (+)
    minus = makeop Number unwrapNum (-)
    times = makeop Number unwrapNum (*)
    divides = makeop Number unwrapNum (/)

evalPrint :: P.Expr -> VMstate Val
evalPrint e = eval e >>= liftIO . print >> pure Nil

evalAssign :: P.Ident -> P.Expr -> VMstate Val
evalAssign id e = do
  env <- get
  if isJust $ envLookup id env
    then eval e >>= modify . envModify id
    else raise $ NotInScope id
  pure Nil

-- | Stateful computation that may fail and may have side effect
-- s: state
-- e: error
-- a: computation result
newtype State s e a = State {runState :: s -> IO (Either e a, s)}

instance Functor (State s e) where
  fmap f comp = State $ fmap (first (fmap f)) . runState comp

instance Applicative (State s e) where
  pure x = State $ \s -> pure (Right x, s)
  sf <*> sx = State $ runState sf >=> cc
    where
      cc res = case res of
        (Right f, s') -> runState (f <$> sx) s'
        (Left e, s') -> pure (Left e, s')

instance Monad (State s e) where
  sx >>= f = State $ runState sx >=> cc
    where
      cc res = case res of
        (Right x, s') -> runState (f x) s'
        (Left e, s') -> pure (Left e, s')

get :: State s e s
get = State $ \s -> pure (Right s, s)

put :: s -> State s e ()
put s = State $ \s -> pure (Right (), s)

modify :: (s -> s) -> State s e ()
modify f = State $ \s -> pure (Right (), f s)

raise :: e -> State s e a
raise e = State $ \s -> pure (Left e, s)

liftIO :: IO a -> State s e a
liftIO io = State $ \s -> do
  x <- io
  pure (Right x, s)
