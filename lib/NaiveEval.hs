module NaiveEval
  ( ValType (..),
    Val (..),
    Env (..),
    VMstate (..),
    run,
    runProg,
    runState,
    initEnv,
  )
where

import qualified AST
import Control.Monad (void, when, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Maybe (fromJust, fromMaybe, isJust)
import GHC.Clock (getMonotonicTime)

data ValType = NilT | BoolT | NumberT | StringT | FuncT deriving (Eq)

type Signature = AST.Ident

type Arity = Int

data Val = Nil | Bool Bool | Number Double | String String | Func Signature Arity ([Val] -> VMstate Val)

instance Eq Val where
  x == y = case (x, y) of
    (Nil, Nil) -> True
    (Bool b, Bool b') -> b == b'
    (Number n, Number n') -> n == n'
    (String s, String s') -> s == s'
    (Func sig _ _, Func sig' _ _) -> sig == sig'
    _ -> False

typeof :: Val -> ValType
typeof Nil = NilT
typeof (Bool _) = BoolT
typeof (Number _) = NumberT
typeof (String _) = StringT
typeof (Func {}) = FuncT

instance Show ValType where
  show NilT = "[type: nil]"
  show BoolT = "[type: bool]"
  show NumberT = "[type: number]"
  show StringT = "[type: string]"
  show FuncT = "[type: function]"

instance Show Val where
  show Nil = "nil"
  show (Bool b) = if b then "true" else "false"
  show (Number n) = show n
  show (String s) = s
  show (Func id _ _) = "<Func: id=" ++ show id ++ ">"

type Assignment = [(AST.Ident, Val)]

assgnHas :: AST.Ident -> Assignment -> Bool
assgnHas id assgn = isJust $ lookup id assgn

assgnUpd :: AST.Ident -> Val -> Assignment -> Assignment
assgnUpd id nv = map $ \(x, v) -> (x, if x == id then nv else v)

data Env = Env
  { assgn :: Assignment,
    parent :: Maybe Env
  }

initEnv =
  Env
    { assgn =
        [ fun "clock" 0 _getClock,
          fun "print" 1 _printVal,
          fun "strcat" 2 _strcat,
          fun "toString" 1 _toStr
        ],
      parent = Nothing
    }
  where
    fun name arity f = let id = AST.Ident name in (id, Func id arity f)

_getClock :: a -> VMstate Val
_getClock = const $ liftIO $ Number <$> getMonotonicTime

_printVal :: [Val] -> VMstate Val
_printVal (v : _) = liftIO $ print v $> Nil

_strcat :: [Val] -> VMstate Val
_strcat (s1 : s2 : _) = fmap String $ (++) <$> unwrapString s1 <*> unwrapString s2

_toStr :: [Val] -> VMstate Val
_toStr (v : _) = pure $ String $ show v

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

data Err = TypeMismatch {expectedT :: ValType, actualT :: ValType} | NotInScope {referred :: AST.Ident} | AlreadyDeclared {redef :: AST.Ident} | ArityMisMatch {expectedArgs :: Int, actualArgs :: Int}

instance Show Err where
  show (TypeMismatch expect actual) = "ERROR: type miss match," ++ "expected " ++ show expect ++ "actual " ++ show actual
  show (NotInScope ref) = "ERROR: " ++ show ref ++ " not in scope"
  show (AlreadyDeclared ref) = "ERROR: " ++ show ref ++ " is already defined"
  show (ArityMisMatch expect actual) = "ERROR: expecting " ++ show expect ++ "args but given " ++ show actual

type VMstate a = State Env Err a

run :: Env -> AST.Prog -> IO (Either Err (), Env)
run env prog = runState (runProg prog) env

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
runDecl (AST.FunDecl name parms body) = runFunDecl name parms body

runStmt :: AST.Stmt -> VMstate ()
runStmt AST.EmptyStmt = pure ()
runStmt (AST.ExprStmt e) = void $ eval e
runStmt (AST.IfStmt cond t f) = runITE cond t f
runStmt (AST.ForStmt init cond next body) = runNested $ do
  runDecl init
  loop
  where
    loop = do
      ok <- eval cond >>= unwrapBool
      when ok $ runStmt body >> runStmt (AST.ExprStmt next) >> loop
runStmt (AST.WhileStmt cond body) = loop
  where
    loop = do
      ok <- eval cond >>= unwrapBool
      when ok $ runStmt body >> loop
runStmt (AST.BlockStmt ss) = runNested $ runDecls ss

runITE :: AST.Expr -> AST.Stmt -> AST.Stmt -> VMstate ()
runITE cond brT brF = do
  c <- eval cond >>= unwrapBool
  let br = if c then brT else brF
  runStmt $ AST.BlockStmt [AST.StmtDecl br]

runNested :: VMstate a -> VMstate a
runNested run = do
  modify envPush
  res <- run
  modify $ fromJust . envPop
  pure res

runVarDecl :: AST.Ident -> Maybe AST.Expr -> VMstate ()
runVarDecl id init =
  do
    env <- get
    when (assgnHas id $ assgn env) $ raise (AlreadyDeclared id)
    iv <- maybe (pure Nil) eval init
    modify $ envAdd id iv

runFunDecl :: AST.Ident -> [AST.Ident] -> AST.Stmt -> VMstate ()
runFunDecl name parms body = do
  env <- get
  when (assgnHas name $ assgn env) $ raise (AlreadyDeclared name)
  modify $
    envAdd name $
      Func name (length parms) $
        \args -> runNested $ do
          foldl (>>) (pure ()) $ zipWith (\p a -> modify $ envAdd p a) parms args
          runStmt body
          pure Nil

-- TODO: return statement

evalAssign :: AST.Ident -> AST.Expr -> VMstate ()
evalAssign id e = do
  env <- get
  if isJust $ envLookup id env
    then eval e >>= modify . envModify id
    else raise $ NotInScope id

unwrapBool :: Val -> VMstate Bool
unwrapBool (Bool b) = pure b
unwrapBool x = raise $ TypeMismatch {expectedT = BoolT, actualT = typeof x}

unwrapNum :: Val -> VMstate Double
unwrapNum (Number n) = pure n
unwrapNum x = raise $ TypeMismatch {expectedT = NumberT, actualT = typeof x}

unwrapString :: Val -> VMstate String
unwrapString (String s) = pure s
unwrapString x = raise $ TypeMismatch {expectedT = StringT, actualT = typeof x}

unwrapNil :: Val -> VMstate ()
unwrapNil Nil = pure ()
unwrapNil x = raise $ TypeMismatch {expectedT = NilT, actualT = typeof x}

eval :: AST.Expr -> VMstate Val
eval (AST.LiteralExpr lit) = evalLiteral lit
eval (AST.UnaryExpr uop sub) = evalUnary uop sub
eval (AST.BinaryExpr bop lhs rhs) = evalBinary bop lhs rhs
eval (AST.AsgnExpr id e) = evalAssign id e $> Nil
eval (AST.CallExpr func args) = evalCall func args

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

evalUnary :: AST.UnaryOp -> AST.Expr -> VMstate Val
evalUnary op sub = eval sub >>= opfunc op
  where
    opfunc :: AST.UnaryOp -> Val -> VMstate Val
    opfunc op = case op of
      AST.Neg -> negf
      AST.Not -> notf
    notf v = Bool . not <$> unwrapBool v
    negf v = Number . (0 -) <$> unwrapNum v

evalBinary :: AST.BinaryOp -> AST.Expr -> AST.Expr -> VMstate Val
evalBinary op lhs rhs = do
  lv <- eval lhs
  rv <- eval rhs
  opfunc op lv rv
  where
    opfunc :: AST.BinaryOp -> Val -> Val -> VMstate Val
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
      AST.And -> and
      AST.Or -> or
    makeop lift unwrap op l r = do
      lv <- unwrap l
      rv <- unwrap r
      pure $ lift $ op lv rv
    eq = makeop Bool pure (==)
    neq = makeop Bool pure (/=)
    lt = makeop Bool unwrapNum (<)
    leq = makeop Bool unwrapNum (<=)
    gt = makeop Bool unwrapNum (>)
    geq = makeop Bool unwrapNum (>=)
    plus = makeop Number unwrapNum (+)
    minus = makeop Number unwrapNum (-)
    times = makeop Number unwrapNum (*)
    divides = makeop Number unwrapNum (/)
    and = makeop Bool unwrapBool (&&)
    or = makeop Bool unwrapBool (||)

evalCall :: AST.Expr -> [AST.Expr] -> VMstate Val
evalCall func args = do
  f <- eval func
  let argsCnt = length args
  case f of
    Func _ arity body ->
      if argsCnt == arity
        then mapM eval args >>= body
        else raise $ ArityMisMatch {expectedArgs = arity, actualArgs = argsCnt}
    _ -> raise $ TypeMismatch {expectedT = FuncT, actualT = typeof f}

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
