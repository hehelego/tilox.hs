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

import qualified AST
import Control.Monad (void, when, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Maybe (fromMaybe, isJust)

data ValType = NilT | BoolT | NumberT | StringT deriving (Eq)

data Val = Nil | Bool Bool | Number Double | String String deriving (Eq)

instance Show ValType where
  show NilT = "[type: nil]"
  show BoolT = "[type: bool]"
  show NumberT = "[type: number]"
  show StringT = "[type: string]"

instance Show Val where
  show Nil = "nil"
  show (Bool b) = if b then "true" else "false"
  show (Number n) = show n
  show (String s) = show s
  show _ = error "unsupported value"

type Assignment = [(AST.Ident, Val)]

assgnHas :: AST.Ident -> Assignment -> Bool
assgnHas id assgn = isJust $ lookup id assgn

assgnUpd :: AST.Ident -> Val -> Assignment -> Assignment
assgnUpd id nv = map $ \(x, v) -> (x, if x == id then nv else v)

data Env = Env
  { assgn :: Assignment,
    parent :: Maybe Env
  }

emptyEnv = Env [] Nothing

envLookup :: AST.Ident -> Env -> Maybe Val
envLookup id env = if isJust cur then cur else par
  where
    cur = lookup id $ assgn env
    par = parent env >>= envLookup id

envModify :: AST.Ident -> Val -> Env -> Env
envModify id v env@(Env assgn parent) =
  if assgnHas id assgn
    then env {assgn = assgnUpd id v assgn}
    else env {parent = envModify id v <$> parent}

envAdd :: AST.Ident -> Val -> Env -> Env
envAdd id val env = env {assgn = (id, val) : assgn env}

envPush :: Env -> Env
envPush env = Env {assgn = [], parent = Just env}

envPop :: Env -> Maybe Env
envPop = parent

data Err = TypeMismatch {expectedT :: ValType, actualT :: ValType} | NotInScope {referred :: AST.Ident} | AlreadyDeclared {redef :: AST.Ident}

instance Show Err where
  show (TypeMismatch exp act) = "ERROR: type miss match," ++ "expected " ++ show exp ++ "actual " ++ show act
  show (NotInScope ref) = "ERROR: " ++ show ref ++ " not in scope"
  show (AlreadyDeclared ref) = "ERROR: " ++ show ref ++ " is already defined"
  show _ = error "unknown error"

type VMstate a = State Env Err a

runDecls :: [AST.Decl] -> VMstate ()
runDecls decls = foldl (>>) init decls'
  where
    init = pure ()
    decls' = runDecl <$> decls

runProg :: AST.Prog -> VMstate ()
runProg (AST.Prog decls) = runDecls decls

runDecl :: AST.Decl -> VMstate ()
runDecl (AST.StmtDecl stmt) = runStmt stmt
runDecl (AST.VarDecl var init) = runVarDecl var init

runStmt :: AST.Stmt -> VMstate ()
runStmt (AST.ExprStmt e) = void $ eval e
runStmt (AST.PrintStmt e) = runPrint e
runStmt (AST.BlockStmt ss) = do
  modify envPush
  runDecls ss
  modify $ \env -> fromMaybe env (envPop env)

runPrint :: AST.Expr -> VMstate ()
runPrint e = eval e >>= liftIO . print

runVarDecl :: AST.Ident -> Maybe AST.Expr -> VMstate ()
runVarDecl id init =
  do
    env <- get
    when (assgnHas id $ assgn env) $ raise (AlreadyDeclared id)
    iv <- maybe (pure Nil) eval init
    modify $ envAdd id iv

evalAssign :: AST.Ident -> AST.Expr -> VMstate ()
evalAssign id e = do
  env <- get
  if isJust $ envLookup id env
    then eval e >>= modify . envModify id
    else raise $ NotInScope id

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

eval :: AST.Expr -> VMstate Val
eval (AST.LiteralExpr lit) = evalLiteral lit
eval (AST.UnaryExpr uop sub) = evalUnary uop sub
eval (AST.BinaryExpr bop lhs rhs) = evalBinary bop lhs rhs
eval (AST.AsgnExpr id e) = evalAssign id e $> Nil
eval _ = error "unsupported expression"

evalLiteral :: AST.Literal -> VMstate Val
evalLiteral (AST.Ref ident) = do
  env <- get
  case envLookup ident env of
    Just v -> pure v
    Nothing -> raise $ NotInScope ident
evalLiteral (AST.Number n) = pure $ Number n
evalLiteral (AST.String s) = pure $ String s
evalLiteral (AST.Bool b) = pure $ Bool b
evalLiteral AST.Nil = pure Nil
evalLiteral _ = error "unsupported literal"

evalUnary :: AST.UnaryOp -> AST.Expr -> VMstate Val
evalUnary op sub = let v = eval sub in opfunc op <$> v
  where
    opfunc op = case op of
      AST.Neg -> negf
      AST.Not -> notf
      _ -> error "unsupported unary operator"
    negf v = Number (-unwrapNum v)
    notf v = Bool (not $ unwrapBool v)

evalBinary :: AST.BinaryOp -> AST.Expr -> AST.Expr -> VMstate Val
evalBinary op lhs rhs =
  let lv = eval lhs; rv = eval rhs
   in opfunc op <$> lv <*> rv
  where
    opfunc op = case op of
      AST.Eq -> eq
      AST.Neq -> neq
      AST.Lt -> lt
      AST.Leq -> leq
      AST.Gt -> gt
      AST.Geq -> geq
      AST.Plus -> plus
      AST.Minus -> minus
      AST.Times -> times
      AST.Divides -> divides
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

instance MonadIO (State s e) where
  liftIO io = State $ \s -> do
    x <- io
    pure (Right x, s)

get :: State s e s
get = State $ \s -> pure (Right s, s)

put :: s -> State s e ()
put s = State $ \s -> pure (Right (), s)

modify :: (s -> s) -> State s e ()
modify f = State $ \s -> pure (Right (), f s)

raise :: e -> State s e a
raise e = State $ \s -> pure (Left e, s)
