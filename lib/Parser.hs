module Parser
  ( Expr (..),
    Literal (..),
    UnaryOp (..),
    BinaryOp (..),
    Parser (..),
    ParseErr (..),
    exprP,
  )
where

import Data.Bifunctor (first)
import Data.Functor (($>))
import qualified Scanner as S

newtype Prog = Prog [Stmt]

type Stmt = Expr

-- | the grammar
-- expression     → literal | unary | binary | grouping ;
-- literal        → NUMBER | STRING | "true" | "false" | "nil" ;
-- grouping       → "(" expression ")" ;
-- unary          → ( "-" | "!" ) expression ;
-- binary         → expression operator expression ;
-- operator       → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;
data Expr
  = LiteralExpr Literal
  | UnaryExpr UnaryOp Expr
  | BinaryExpr BinaryOp Expr Expr

data Literal = Number Double | String String | Bool Bool | Nil

data UnaryOp = Neg | Not

data BinaryOp = Eq | Neq | Lt | Leq | Gt | Geq | Plus | Minus | Times | Divides

instance Show Expr where
  show (LiteralExpr lit) = show lit
  show (UnaryExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
  show (BinaryExpr op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"

instance Show Literal where
  show (Number n) = show n
  show (String s) = show s
  show (Bool b) = if b then "true" else "false"
  show Nil = "nil"

instance Show UnaryOp where
  show Neg = "-"
  show Not = "!"

instance Show BinaryOp where
  show Eq = "=="
  show Neq = "!="
  show Lt = "<"
  show Leq = "<="
  show Gt = ">"
  show Geq = ">="
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divides = "/"

-- | Parse an expression tree
chainP :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
chainP leaveP connP = leaveP >>= chain
  where
    chain lhs =
      ( do
          conn <- connP
          rhs <- leaveP
          chain $ conn lhs rhs
      )
        `orElse` pure lhs

exprP, equalityP, compP, termP, factorP, unaryP, primaryP, numberP, stringP, boolP, nilP, groupedP :: Parser Expr

-- | expression     → equality ;
exprP = equalityP `orElse` failP "no expression"

-- | equality       → comparison ( ( "!=" | "==" ) comparison )* ;
equalityP = chainP compP (binaryOpP [S.EQUAL_EQUAL, S.BANG_EQUAL]) `orElse` failP "no equality expression"

-- | comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
compP = chainP termP (binaryOpP [S.GREATER, S.GREATER_EQUAL, S.LESS, S.LESS_EQUAL]) `orElse` failP "no comparison expression"

-- | term           → factor ( ( "-" | "+" ) factor )* ;
termP = chainP factorP (binaryOpP [S.MINUS, S.PLUS]) `orElse` failP "no term expression"

-- | factor         → unary ( ( "/" | "*" ) unary )* ;
factorP = chainP unaryP (binaryOpP [S.SLASH, S.STAR]) `orElse` failP "no factor expression"

-- | unary          → ( "!" | "-" ) unary | primary ;
unaryP = (unaryOpP <*> unaryP) `orElse` primaryP `orElse` failP "no unary expression"

-- | primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
primaryP = numberP `orElse` stringP `orElse` boolP `orElse` nilP `orElse` groupedP `orElse` failP "no primary expression"

literalP tp lift = LiteralExpr . lift . S.tokRaw <$> takeType tp

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

groupedP = left *> exprP <* right
  where
    left = takeType S.LEFT_PAREN
    right = takeType S.RIGHT_PAREN

type ErrMsg = String

newtype ParseErr = ParseErr {errMsg :: ErrMsg} deriving (Show)

newtype Parser a = Parser {runParser :: [S.Token] -> (Either ParseErr a, [S.Token])}

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

-- | consume a token
consume :: Parser ()
consume = Parser $ \toks -> (Right (), tail toks)

-- | consume a token and check if a predicate on the token type is satisfied
takeCheck :: (S.Type -> Bool) -> ErrMsg -> Parser S.Token
takeCheck pred err = do
  token <- peek
  checkP (pred $ S.tokType token) err
  consume $> token

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

instance Functor Parser where
  fmap f p = Parser $ \toks -> (f <$>) `first` runParser p toks

instance Applicative Parser where
  pure x = Parser $ \toks -> (Right x, toks)
  p <*> q = Parser $ \toks -> case runParser p toks of
    (Left e, toks') -> (Left e, toks')
    (Right f, toks') -> runParser (f <$> q) toks'

instance Monad Parser where
  p >>= q = Parser $ \toks -> case runParser p toks of
    (Left e, toks') -> (Left e, toks')
    (Right x, toks') -> runParser (q x) toks'

-- | Parse the input with either the first or the second parser.
-- If the first parser fails to consume any token, then parse the token stream with the second parser
orElse :: Parser a -> Parser a -> Parser a
p `orElse` q = Parser $ \toks -> case runParser p toks of
  (Left e, toks') -> if toks == toks' then runParser q toks else (Left e, toks')
  (Right x, toks') -> (Right x, toks')
