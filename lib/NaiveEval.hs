module NaiveEval (
    ValType (..),
    Val (..),
    Env (..),
    VMstate (..),
    run,
    runProg,
    runState,
    initEnv,
)
where

import AST (
    BinaryOp (..),
    Decl (..),
    Expr (..),
    Ident (..),
    Literal,
    Prog (..),
    Stmt (..),
    UnaryOp (..),
 )
import qualified AST
import Control.Monad (when, (>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.Maybe (fromJust, fromMaybe, isJust)
import GHC.Clock (getMonotonicTime)
import qualified PList

data ValType = NilT | BoolT | NumberT | StringT | FuncT deriving (Eq)

type Arity = Int
type ScopePtr = PList.Addr

data Val = Nil | Bool Bool | Number Double | String String | Func ScopePtr Ident Arity ([Val] -> VMstate Val)

instance Eq Val where
    x == y = case (x, y) of
        (Nil, Nil) -> True
        (Bool b, Bool b') -> b == b'
        (Number n, Number n') -> n == n'
        (String s, String s') -> s == s'
        (Func scope sig _ _, Func scope' sig' _ _) -> scope == scope && sig == sig'
        _ -> False

typeof :: Val -> ValType
typeof Nil = NilT
typeof (Bool _) = BoolT
typeof (Number _) = NumberT
typeof (String _) = StringT
typeof (Func{}) = FuncT

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
    show (Func scope name _ _) = "<Func@" ++ show scope ++ " :" ++ show name ++ ">"

type Assoc k v = [(k, v)]

assocUpd :: (Eq k) => k -> v -> Assoc k v -> Assoc k v
assocUpd k nv [] = []
assocUpd k nv ((x, v) : assoc) =
    if k == x
        then (x, nv) : assoc
        else (x, v) : assocUpd k nv assoc

assocHasKey :: (Eq k) => k -> Assoc k v -> Bool
assocHasKey k assoc = k `elem` (fst <$> assoc)

type Env = PList.List (Assoc Ident Val)

inActiveScope :: Ident -> Env -> Bool
inActiveScope id env = assocHasKey id $ PList.this $ PList.getActive env

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

envCurScope :: Env -> ScopePtr
envCurScope = PList.active

envJump :: ScopePtr -> Env -> Env
envJump scope env = env{PList.active = scope}

envLookup :: Ident -> Env -> Maybe Val
envLookup id env = find $ PList.getActive env
  where
    find (PList.Node prev this) = lookup id this `orElse` findPar prev
    findPar prev = do
        addr <- prev
        find $ PList.get addr env

envModify :: Ident -> Val -> Env -> Env
envModify id v env =
    let top = envCurScope env
        addr = fromJust $ find top $ PList.get top env
     in PList.upd addr upd env
  where
    find addr (PList.Node prev this) =
        if assocHasKey id this
            then Just addr
            else findPar prev
    findPar prev = do
        addr <- prev
        find addr $ PList.get addr env
    upd = assocUpd id v

envBind :: Ident -> Val -> Env -> Env
envBind id val = PList.updActive ((id, val) :)

envPush :: Env -> Env
envPush = PList.push []

envPop :: Env -> Maybe Env
envPop = PList.pop

initEnv :: Env
initEnv =
    PList.singleton
        [ fun "clock" 0 clock
        , fun "print" 1 print'
        , fun "strcat" 2 strcat
        , fun "toString" 1 toString
        ]
  where
    rootScope = 0
    fun name arity f = let id = Ident name in (id, Func rootScope id arity f)

    clock :: a -> VMstate Val
    clock = const $ liftIO $ Number <$> getMonotonicTime

    print' :: [Val] -> VMstate Val
    print' (v : _) = liftIO $ print v $> Nil

    strcat :: [Val] -> VMstate Val
    strcat (s1 : s2 : _) = fmap String $ (++) <$> unwrapString s1 <*> unwrapString s2

    toString :: [Val] -> VMstate Val
    toString (v : _) = pure $ String $ show v

data Err = TypeMismatch {expectedT :: ValType, actualT :: ValType} | NotInScope {referred :: Ident} | AlreadyDeclared {redef :: Ident} | ArityMisMatch {expectedArgs :: Int, actualArgs :: Int}

instance Show Err where
    show (TypeMismatch expect actual) = "ERROR: type miss match," ++ "expected " ++ show expect ++ "actual " ++ show actual
    show (NotInScope ref) = "ERROR: " ++ show ref ++ " not in scope"
    show (AlreadyDeclared ref) = "ERROR: " ++ show ref ++ " is already defined"
    show (ArityMisMatch expect actual) = "ERROR: expecting " ++ show expect ++ "args but given " ++ show actual

type VMstate a = State Env Err a

{- | represent the control flow direction
 - return with value or continue with value
-}
data FlowDirection = Continue | Return deriving (Show, Eq)

newtype Result = Result (FlowDirection, Val) deriving (Show, Eq)

wrapContinue, wrapReturn :: Val -> Result
wrapContinue x = Result (Continue, x)
wrapReturn x = Result (Return, x)

continueWith, returnWith :: (Applicative m) => Val -> m Result
continueWith = pure . wrapContinue
returnWith = pure . wrapReturn

valueOf :: Result -> Val
valueOf (Result (_, val)) = val

dirOf :: Result -> FlowDirection
dirOf (Result (dir, _)) = dir

run :: Env -> Prog -> IO (Either Err Result, Env)
run env prog = runState (runProg prog) env

runSeq :: [VMstate Result] -> VMstate Result
runSeq = foldl seq $ continueWith Nil
  where
    seq :: VMstate Result -> VMstate Result -> VMstate Result
    seq acc x = do
        prev <- acc
        case dirOf prev of
            Continue -> x
            Return -> pure prev

runDecls :: [Decl] -> VMstate Result
runDecls decls = runSeq $ runDecl <$> decls

runProg :: Prog -> VMstate Result
runProg (Prog decls) = runDecls decls

runDecl :: Decl -> VMstate Result
runDecl (StmtDecl stmt) = runStmt stmt
runDecl (VarDecl var init) = runVarDecl var init
runDecl (FunDecl name parms body) = runFunDecl name parms body

runStmt :: Stmt -> VMstate Result
runStmt EmptyStmt = continueWith Nil
runStmt (ExprStmt e) = wrapContinue <$> eval e
runStmt (IfStmt cond t f) = runITE cond t f
runStmt (ForStmt init cond next body) = runNested $ runDecl init >> loop
  where
    loop = do
        cond <- eval cond
        continue <- unwrapBool cond
        if continue
            then
                runStmt body >> runStmt (ExprStmt next) >> loop
            else
                continueWith Nil
runStmt (WhileStmt cond body) = loop
  where
    loop = do
        cond <- eval cond
        continue <- unwrapBool cond
        if continue
            then runStmt body >> loop
            else continueWith Nil
runStmt (BlockStmt ss) = runNested $ runDecls ss
runStmt (ReturnStmt e) = wrapReturn <$> eval e

runITE :: Expr -> AST.Stmt -> AST.Stmt -> VMstate Result
runITE cond brT brF = do
    cond <- eval cond
    side <- unwrapBool cond
    runNested $ runStmt $ if side then brT else brF

runNested :: VMstate a -> VMstate a
runNested run = do
    modify envPush
    res <- run
    env <- get
    modify $ fromJust . envPop
    pure res

runVarDecl :: Ident -> Maybe Expr -> VMstate Result
runVarDecl id init = do
    env <- get
    let inScope = inActiveScope id env
    when inScope $ raise (AlreadyDeclared id)
    init <- maybe (pure Nil) eval init
    modify $ envBind id init
    continueWith Nil

runFunDecl :: Ident -> [Ident] -> [Decl] -> VMstate Result
runFunDecl name parms body = do
    env <- get
    let inScope = inActiveScope name env
    when inScope $ raise (AlreadyDeclared name)
    let scope = envCurScope env
    let fn = Func scope name (length parms) f
    modify $ envBind name fn
    continueWith Nil
  where
    f args = runNested $ do
        -- set parameters
        runSeq $ zipWith evalParm parms args
        -- run the function body, and return the last value
        valueOf <$> runDecls body

    evalParm parm arg = modify (envBind parm arg) >> continueWith Nil

evalAssign :: Ident -> Expr -> VMstate Val
evalAssign id e = do
    env <- get
    if isJust $ envLookup id env
        then do
            v <- eval e
            modify $ envModify id v
            pure v
        else raise $ NotInScope id

unwrapBool :: Val -> VMstate Bool
unwrapBool (Bool b) = pure b
unwrapBool x = raise TypeMismatch{expectedT = BoolT, actualT = typeof x}

unwrapNum :: Val -> VMstate Double
unwrapNum (Number n) = pure n
unwrapNum x = raise TypeMismatch{expectedT = NumberT, actualT = typeof x}

unwrapString :: Val -> VMstate String
unwrapString (String s) = pure s
unwrapString x = raise TypeMismatch{expectedT = StringT, actualT = typeof x}

unwrapNil :: Val -> VMstate ()
unwrapNil Nil = pure ()
unwrapNil x = raise TypeMismatch{expectedT = NilT, actualT = typeof x}

unwrapFunc :: Val -> VMstate (ScopePtr, Ident, Arity, [Val] -> VMstate Val)
unwrapFunc (Func scope name arity f) = pure (scope, name, arity, f)
unwrapFunc x = raise TypeMismatch{expectedT = FuncT, actualT = typeof x}

eval :: Expr -> VMstate Val
eval (LiteralExpr lit) = evalLiteral lit
eval (UnaryExpr uop sub) = evalUnary uop sub
eval (BinaryExpr bop lhs rhs) = evalBinary bop lhs rhs
eval (AsgnExpr id e) = evalAssign id e $> Nil
eval (CallExpr func args) = evalCall func args

evalLiteral :: Literal -> VMstate Val
evalLiteral (AST.Ref ident) = do
    env <- get
    case envLookup ident env of
        Just v -> pure v
        Nothing -> raise $ NotInScope ident
evalLiteral (AST.Number n) = pure $ Number n
evalLiteral (AST.String s) = pure $ String s
evalLiteral (AST.Bool b) = pure $ Bool b
evalLiteral AST.Nil = pure Nil

evalUnary :: UnaryOp -> AST.Expr -> VMstate Val
evalUnary op sub = eval sub >>= opfunc op
  where
    opfunc :: UnaryOp -> Val -> VMstate Val
    opfunc op = case op of
        Neg -> negf
        Not -> notf
    notf v = Bool . not <$> unwrapBool v
    negf v = Number . (0 -) <$> unwrapNum v

evalBinary :: BinaryOp -> AST.Expr -> AST.Expr -> VMstate Val
evalBinary op lhs rhs = do
    lv <- eval lhs
    rv <- eval rhs
    opfunc op lv rv
  where
    opfunc :: BinaryOp -> Val -> Val -> VMstate Val
    opfunc op = case op of
        Eq -> eq
        Neq -> neq
        Lt -> lt
        Leq -> leq
        Gt -> gt
        Geq -> geq
        Plus -> plus
        Minus -> minus
        Times -> times
        Divides -> divides
        And -> and
        Or -> or
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

evalCall :: Expr -> [AST.Expr] -> VMstate Val
evalCall func args = do
    f <- eval func
    (scope, name, arity, body) <- unwrapFunc f
    let cnt = length args
    when (arity /= cnt) $ raise ArityMisMatch{expectedArgs = arity, actualArgs = cnt}
    -- evaluate arguments
    save <- envCurScope <$> get
    argVals <- mapM eval args
    -- run function in the lexical scope where it is declared
    modify $ envJump scope
    retVal <- body argVals
    -- jump back to the saved scope
    modify $ envJump save
    pure retVal

{- | Stateful computation that may fail and may have side effect
s: state
e: error
a: computation result
-}
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
