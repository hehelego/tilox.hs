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
  show (Bool b) = show b
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
exprP = equalityP

-- | equality       → comparison ( ( "!=" | "==" ) comparison )* ;
equalityP = chainP compP $ binaryOpP [S.EQUAL_EQUAL, S.BANG_EQUAL]

-- | comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
compP = chainP termP $ binaryOpP [S.GREATER, S.GREATER_EQUAL, S.LESS, S.LESS_EQUAL]

-- | term           → factor ( ( "-" | "+" ) factor )* ;
termP = chainP factorP $ binaryOpP [S.MINUS, S.PLUS]

-- | factor         → unary ( ( "/" | "*" ) unary )* ;
factorP = chainP unaryP $ binaryOpP [S.SLASH, S.STAR]

-- | unary          → ( "!" | "-" ) unary | primary ;
unaryP = (unaryOpP <*> unaryP) `orElse` primaryP

-- | primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
primaryP = numberP `orElse` stringP `orElse` boolP `orElse` nilP `orElse` groupedP

literalP tp lift = do
  token <- takeType tp
  pure $ LiteralExpr $ lift $ S.tokRaw token

numberP = literalP S.NUMBER (Number . read)

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
  let err = ParseErr $ case toks of
        t : _ -> "Error: " ++ msg ++ " at " ++ show t
        [] -> "Error: " ++ msg ++ " at end of file"
   in (Left err, toks)

-- | A parser that only checks if a condition is hold
checkP :: Bool -> ErrMsg -> Parser ()
checkP cond err = if cond then pure () else failP err

-- | consume a token
consume :: Parser S.Token
consume = Parser $ \toks -> case toks of
  (t : rest) -> (Right t, rest)
  [] -> (Left $ ParseErr "Error: token stream ended", [])

-- | consume a token and check if a predicate on the token type is satisfied
takeCheck :: (S.Type -> Bool) -> ErrMsg -> Parser S.Token
takeCheck pred err = do
  token <- consume
  checkP (pred $ S.tokType token) err
  pure token

-- | consume a token and check if the token type matches with the provided type
takeType :: S.Type -> Parser S.Token
takeType t = takeCheck (== t) $ "expecting a " ++ show t

binaryOps, unaryOps :: [S.Type]
unaryOps = [S.MINUS, S.BANG]
binaryOps = [S.EQUAL_EQUAL, S.BANG_EQUAL, S.LESS, S.LESS_EQUAL, S.GREATER, S.GREATER_EQUAL, S.PLUS, S.MINUS, S.STAR, S.SLASH]

unaryOpP :: Parser (Expr -> Expr)
unaryOpP = do
  token <- takeCheck (`elem` unaryOps) "expecting an unary operator token"
  pure $ UnaryExpr $ case S.tokType token of
    S.MINUS -> Neg
    S.BANG -> Not

binaryOpP :: [S.Type] -> Parser (Expr -> Expr -> Expr)
binaryOpP opTypes = do
  token <- takeCheck (`elem` opTypes) "expecting a binary operator token"
  let t = S.tokType token
  pure $ BinaryExpr $ case t of
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
-- The second parser is provided with the entire input if the first parser fails.
orElse :: Parser a -> Parser a -> Parser a
p `orElse` q = Parser $ \toks -> case runParser p toks of
  (Left _, _) -> runParser q toks
  (Right x, toks') -> (Right x, toks')
