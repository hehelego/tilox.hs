module Parser
  ( Parser (..),
    ParseErr (..),
    parse,
    exprP,
    stmtP,
    declP,
    progP,
  )
where

import AST
import Data.Bifunctor (first)
import Data.Functor (($>))
import qualified Scanner as S

-- | user facing interface: parse a lox program from a token stream
parse :: [S.Token] -> (Either ParseErr (), AST.Prog)
parse toks = case fst $ runParser progP toks of
  Left e -> (Left e, AST.Prog [])
  Right prog -> (Right (), prog)

-- expression parser

exprP, asgnP, logicalOrP, logicalAndP, equalityP, compP, termP, factorP, unaryP, callP, primaryP, identRefP, numberP, stringP, boolP, nilP :: Parser Expr

-- | expression     → assignment ;
exprP = asgnP `orElse` failP "no expression"

-- | assignment     → IDENTIFIER "=" assignment | logical_or ;
asgnP = do
  lhs <- logicalOrP
  op <- peek
  case (lhs, S.tokType op) of
    (LiteralExpr (Ref id), S.EQUAL) -> consume >> AsgnExpr id <$> asgnP
    _ -> pure lhs

-- | logic_or       → logic_and ( "or" logic_and )* ;
logicalOrP = chainP logicalAndP (binaryOpP [S.OR]) `orElse` failP "no logical or expression"

-- | logic_and      → equality ( "and" equality )* ;
logicalAndP = chainP equalityP (binaryOpP [S.AND]) `orElse` failP "no logical and expression"

-- | equality       → comparison ( ( "!=" | "==" ) comparison )* ;
equalityP = chainP compP (binaryOpP [S.EQUAL_EQUAL, S.BANG_EQUAL]) `orElse` failP "no equality expression"

-- | comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
compP = chainP termP (binaryOpP [S.GREATER, S.GREATER_EQUAL, S.LESS, S.LESS_EQUAL]) `orElse` failP "no comparison expression"

-- | term           → factor ( ( "-" | "+" ) factor )* ;
termP = chainP factorP (binaryOpP [S.MINUS, S.PLUS]) `orElse` failP "no term expression"

-- | factor         → unary ( ( "/" | "*" ) unary )* ;
factorP = chainP unaryP (binaryOpP [S.SLASH, S.STAR]) `orElse` failP "no factor expression"

-- | unary          → ( "!" | "-" ) unary | call ;
unaryP = (unaryOpP <*> unaryP) `orElse` callP `orElse` failP "no unary expression"

-- | call           → primary ( "(" arguments? ")" )* ;
-- arguments      → expression ( "," expression )* ;
callP =
  do
    p <- primaryP
    foldlP invokeP CallExpr p
  where
    invokeP = groupedP $ commaSepP exprP

-- | primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
primaryP = numberP `orElse` stringP `orElse` boolP `orElse` nilP `orElse` groupedP exprP `orElse` identRefP `orElse` failP "no primary expression"

literalP tp lift = LiteralExpr . lift . S.tokRaw <$> takeType tp

identRefP = LiteralExpr . Ref <$> identP

identP = parseId <$> takeType S.IDENTIFIER
  where
    parseId t = Ident $ S.tokRaw t

numberP = literalP S.NUMBER (Number . read . fix)
  where
    fix s = if last s == '.' then s ++ "0" else s

stringP = literalP S.STRING (String . read)

boolP = literalP S.TRUE readBool `orElse` literalP S.FALSE readBool
  where
    readBool s = Bool $ case s of
      "true" -> True
      "false" -> False
      _ -> error "a boolean token that is not true nor false"

nilP = literalP S.NIL (const Nil)

eofP :: Parser ()
eofP = takeType S.EOF $> ()

groupedP :: Parser a -> Parser a
groupedP p = left *> p <* right
  where
    left = takeType S.LEFT_PAREN
    right = takeType S.RIGHT_PAREN

semicolonP :: Parser ()
semicolonP = takeType S.SEMICOLON $> ()

-- program parser

stmtP, emptyStP, exprStP, blockStP, ifP, whileP, forP, returnP :: Parser Stmt
stmtP = emptyStP `orElse` blockStP `orElse` ifP `orElse` forP `orElse` whileP `orElse` returnP `orElse` exprStP
emptyStP = semicolonP $> EmptyStmt
blockStP = BlockStmt <$> blockP
ifP = do
  takeType S.IF
  cond <- groupedP exprP
  brT <- stmtP
  elseKw <- peek
  brF <-
    if S.tokType elseKw == S.ELSE
      then consume >> stmtP
      else pure EmptyStmt
  pure $ IfStmt cond brT brF
whileP = do
  takeType S.WHILE
  cond <- groupedP exprP
  WhileStmt cond <$> stmtP
forP = do
  takeType S.FOR
  (init, cond, next) <- groupedP $ do
    init <- initP
    semicolonP
    cond <- exprP
    semicolonP
    next <- exprP
    pure (init, cond, next)
  ForStmt init cond next <$> stmtP
  where
    exprInit = StmtDecl . ExprStmt <$> exprP
    initP = varDeclP `orElse` exprInit
returnP = fmap ReturnStmt $ takeType S.RETURN *> exprP <* semicolonP

-- | block -> { decl* }
blockP :: Parser [Decl]
blockP = takeType S.LEFT_BRACE *> many declP <* takeType S.RIGHT_BRACE

-- | expressionStmt -> expression ";"
exprStP = ExprStmt <$> exprP <* semicolonP

declP, varDeclP, funDeclP :: Parser Decl
varDeclP = VarDecl <$> var <*> init'
  where
    var = takeType S.VAR *> identP
    init = takeType S.EQUAL *> exprP
    init' = fmap Just init `orElse` pure Nothing
funDeclP = FunDecl <$> fn <*> args <*> body
  where
    fn = takeType S.FUN *> identP
    args = groupedP $ commaSepP identP
    body = blockP
declP = var `orElse` fun `orElse` stmt
  where
    stmt = StmtDecl <$> stmtP
    var = varDeclP <* semicolonP
    fun = funDeclP

progP :: Parser Prog
progP = Prog <$> decls
  where
    decls = many declP <* eofP

failP :: ErrMsg -> Parser a
failP msg = Parser $ \toks ->
  let err =
        ParseErr $
          msg ++ codeloc toks
   in (Left err, toks)
  where
    codeloc [] = " past EOF"
    codeloc (t : _) = " at " ++ show t

-- | A parser that only checks if a condition is hold
checkP :: Bool -> ErrMsg -> Parser ()
checkP cond err = if cond then pure () else failP err

-- | peek the next token
peek :: Parser S.Token
peek = Parser $ \toks -> case toks of
  (t : _) -> (Right t, toks)
  [] -> (Left $ ParseErr "token stream ended", [])

-- | peek the next token and check if a predicate is satisfied
peekCheck :: (S.Type -> Bool) -> ErrMsg -> Parser S.Token
peekCheck pred err = do
  token <- peek
  if pred $ S.tokType token
    then pure token
    else failP err

-- | peek a token of a given type
peekType :: S.Type -> Parser S.Token
peekType t = peekCheck (== t) $ "expecting a " ++ show t

-- | consume a token
consume :: Parser S.Token
consume = Parser $ \(t : toks) -> (Right t, toks)

-- | consume a token and check if a predicate on the token type is satisfied
takeCheck :: (S.Type -> Bool) -> ErrMsg -> Parser S.Token
takeCheck pred err = do
  token <- peek
  checkP (pred $ S.tokType token) err
  consume

-- | consume a token and check if the token type matches with the provided type
takeType :: S.Type -> Parser S.Token
takeType t = takeCheck (== t) $ "expecting a " ++ show t

binaryOps, unaryOps :: [S.Type]
unaryOps = [S.MINUS, S.BANG]
binaryOps = [S.EQUAL_EQUAL, S.BANG_EQUAL, S.LESS, S.LESS_EQUAL, S.GREATER, S.GREATER_EQUAL, S.PLUS, S.MINUS, S.STAR, S.SLASH]

unaryOpP :: Parser (Expr -> Expr)
unaryOpP = parseOp <$> takeCheck (`elem` unaryOps) "expecting an unary operator token"
  where
    parseOp token = UnaryExpr $ case S.tokType token of
      S.MINUS -> Neg
      S.BANG -> Not

binaryOpP :: [S.Type] -> Parser (Expr -> Expr -> Expr)
binaryOpP opTypes = parseOp <$> takeCheck (`elem` opTypes) "expecting a binary operator token"
  where
    parseOp token = BinaryExpr $ case S.tokType token of
      S.EQUAL_EQUAL -> Eq
      S.BANG_EQUAL -> Neq
      S.LESS -> Lt
      S.LESS_EQUAL -> Leq
      S.GREATER -> Gt
      S.GREATER_EQUAL -> Geq
      S.PLUS -> Plus
      S.MINUS -> Minus
      S.STAR -> Times
      S.SLASH -> Divides
      S.AND -> And
      S.OR -> Or

-- parser definition section

type ErrMsg = String

newtype ParseErr = ParseErr {errMsg :: ErrMsg} deriving (Show)

newtype Parser a = Parser {runParser :: [S.Token] -> (Either ParseErr a, [S.Token])}

instance Functor Parser where
  fmap f p = Parser $ \toks -> fmap f `first` runParser p toks

instance Applicative Parser where
  pure x = Parser $ (,) (Right x)
  p <*> q = Parser $ \toks -> case runParser p toks of
    (Left e, toks') -> (Left e, toks')
    (Right f, toks') -> runParser (f <$> q) toks'

instance Monad Parser where
  p >>= q = Parser $ \toks -> case runParser p toks of
    (Left e, toks') -> (Left e, toks')
    (Right x, toks') -> runParser (q x) toks'

-- combinators

-- | Parse the input with either the first or the second parser.
-- If the first parser fails to consume any token, then parse the token stream with the second parser
orElse :: Parser a -> Parser a -> Parser a
p `orElse` q = Parser $ \toks -> case runParser p toks of
  (Left e, toks') -> if toks == toks' then runParser q toks else (Left e, toks')
  (Right x, toks') -> (Right x, toks')

foldlP :: Parser a -> (b -> a -> b) -> b -> Parser b
foldlP unit combine init =
  ( do
      fst <- unit
      foldlP unit combine $ combine init fst
  )
    `orElse` pure init

-- | Parse an expression tree
chainP :: Parser a -> Parser (a -> a -> a) -> Parser a
chainP leaveP connP = leaveP >>= chain
  where
    chain lhs =
      ( do
          conn <- connP
          rhs <- leaveP
          chain $ conn lhs rhs
      )
        `orElse` pure lhs

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) `orElse` pure []

commaSepP :: Parser a -> Parser [a]
commaSepP unit = nonEmpty `orElse` pure []
  where
    nonEmpty = (:) <$> unit <*> many (takeType S.COMMA *> unit)
