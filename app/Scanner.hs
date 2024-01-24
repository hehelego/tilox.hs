module Scanner (start, stop) where

import Control.Monad (foldM)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (fromMaybe)

data Type
  = -- symbols
    LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  | -- identifier
    IDENTIFIER
  | -- literals
    STRING
  | NUMBER
  | -- keywords
    AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE
  | -- end of file
    EOF
  deriving (Show, Eq, Ord)

keywords :: [(String, Type)]
keywords =
  [ ("and", AND),
    ("class", CLASS),
    ("else", ELSE),
    ("false", FALSE),
    ("for", FOR),
    ("fun", FUN),
    ("if", IF),
    ("nil", NIL),
    ("or", OR),
    ("print", PRINT),
    ("return", RETURN),
    ("super", SUPER),
    ("this", THIS),
    ("true", TRUE),
    ("var", VAR),
    ("while", WHILE)
  ]

kwidType :: String -> Type
kwidType raw = fromMaybe IDENTIFIER (lookup raw keywords)

data CodeLoc = CodeLoc Int Int
  deriving (Show, Eq, Ord)

data CodeRng = CodeRng CodeLoc CodeLoc
  deriving (Show, Eq, Ord)

data Token = Token {tokRaw :: String, tokRng :: CodeRng, tokType :: Type}
  deriving (Show, Eq, Ord)

data Input = Feed Char | Stop
  deriving (Show, Eq, Ord)

newtype ScanErr = ScanErr {errReason :: String}
  deriving (Show, Eq, Ord)

type ScanRes = Either ScanErr [Token]

failedScan :: String -> ScanRes
failedScan msg = Left $ ScanErr msg

-- Give the (location, input character)
-- 1. Continue scanning, or 2. Stop with (error | token)
newtype Scan = Scan {scanFn :: CodeLoc -> Input -> Either ScanRes Scan}

-- basically the state monad
data Scanner = Scanner {state :: CodeLoc, trans :: Scan}

scanStep :: Scanner -> Char -> Either ScanRes Scanner
scanStep (Scanner state trans) ch = case scanFn trans state (Feed ch) of
  Left res -> Left res
  Right trans' -> Right $ Scanner state' trans'
    where
      state' = nextLoc state ch

scan :: Scanner -> String -> Either ScanRes Scanner
scan scanner = foldl step (Right scanner)
  where
    step (Right scanner) ch = scanStep scanner ch
    step x _ = x

stop :: Scanner -> ScanRes
stop (Scanner state trans) = case scanFn trans state Stop of
  Left r -> (token "" state state EOF :) <$> r
  Right _ -> Left $ ScanErr "scanning cannot be terminated by EOF"

start :: Scanner
start = Scanner {state = CodeLoc 0 0, trans = beginScan}

beginScan :: Scan
beginScan = Scan beginS

beginS :: CodeLoc -> Input -> Either ScanRes Scan
beginS loc Stop = Left $ Right []
beginS loc (Feed ch)
  -- a single char symbol
  | ch == '(' = push1 LEFT_PAREN
  | ch == ')' = push1 RIGHT_PAREN
  | ch == '{' = push1 LEFT_BRACE
  | ch == '}' = push1 RIGHT_BRACE
  | ch == ',' = push1 COMMA
  | ch == '.' = push1 DOT
  | ch == '-' = push1 MINUS
  | ch == '+' = push1 PLUS
  | ch == ';' = push1 SEMICOLON
  | ch == '*' = push1 STAR
  -- a double char symbol
  | ch == '!' = push2 '=' BANG_EQUAL BANG
  | ch == '=' = push2 '=' EQUAL_EQUAL EQUAL
  | ch == '<' = push2 '=' LESS_EQUAL LESS
  | ch == '>' = push2 '=' GREATER_EQUAL GREATER
  -- the start of a comment line or a division symbol
  | ch == '/' = Right $ lookAhead (== Feed '/') (skipUntil (== Feed '\n') beginScan) (push' SLASH beginScan)
  -- a string
  | ch == '"' = Right $ stringScan loc ""
  -- a number
  | isDigit ch = Right $ numScan loc [ch]
  -- white spaces
  | isSpace ch = skip
  -- an identifier or a keyword
  | isAlpha ch || ch == '_' = Right $ identKwScan loc [ch]
  -- a unrecognized character
  | otherwise = Left $ failedScan ("unrecognized character: " ++ [ch] ++ " at " ++ show loc)
  where
    skip = Right beginScan
    push' tokType = pushToken (token [ch] loc loc tokType)
    push1 tokType = Right $ push' tokType beginScan
    push2 ch matched failed = Right $ lookAhead (== Feed ch) (push' matched beginScan) (push' failed beginScan)

stringScan :: CodeLoc -> String -> Scan
stringScan leftLoc acc = Scan $ stringS acc
  where
    stringS :: String -> CodeLoc -> Input -> Either ScanRes Scan
    stringS acc loc Stop = Left $ failedScan ("unexpected EOF while parsing a string starting at " ++ show loc)
    stringS acc loc (Feed ch) =
      Right $
        if ch /= '"'
          then stringScan leftLoc (append acc ch)
          else pushToken (token (quote acc) leftLoc loc STRING) beginScan

numScan :: CodeLoc -> String -> Scan
numScan leftLoc acc = Scan $ numS acc
  where
    numS :: String -> CodeLoc -> Input -> Either ScanRes Scan
    numS acc loc Stop = Left $ Right [token acc leftLoc loc NUMBER]
    numS acc loc (Feed ch) = Right $ lookAhead isDigit' contNumScan beginScan
    contNumScan = Scan $ \loc ch -> let Feed ch' = ch in Right $ numScan leftLoc (append acc ch')
    isDigit' Stop = False
    isDigit' (Feed ch) = isDigit ch

identKwScan :: CodeLoc -> String -> Scan
identKwScan leftLoc acc = Scan $ idkwS acc
  where
    idkwS :: String -> CodeLoc -> Input -> Either ScanRes Scan
    idkwS acc loc Stop = Left $ Right [token acc leftLoc loc (kwidType acc)]
    idkwS acc loc (Feed ch) = Right $ lookAhead isAlpha' contIdkwScan beginScan
    contIdkwScan = Scan $ \loc ch -> let Feed ch' = ch in Right $ identKwScan leftLoc (append acc ch')
    isAlpha' Stop = False
    isAlpha' (Feed ch) = isAlpha ch

-- FIXME: Wait, this is not correct.
-- if lookAhead failed, we should output and continue scan with beginScan

lookAhead :: (Input -> Bool) -> Scan -> Scan -> Scan
lookAhead cond continue fallback = Scan $ \loc ch ->
  if cond ch
    then scanFn continue loc ch
    else scanFn fallback loc ch

-- skip input characters, until the condition is met.
-- resume scanning from the first character that satisfies the condition
skipUntil :: (Input -> Bool) -> Scan -> Scan
skipUntil cond next = Scan $ \loc ch ->
  if cond ch
    then scanFn next loc ch
    else Right $ skipUntil cond next

-- skip input characters, until the condition is met.
-- resume scanning scan from the character next to the first character that satisfies the condition
skipUntil' :: (Input -> Bool) -> Scan -> Scan
skipUntil' cond next = Scan $ \loc ch ->
  if cond ch
    then Right next
    else Right $ skipUntil cond next

pushToken :: Token -> Scan -> Scan
pushToken token (Scan trans) = Scan $ \loc ch -> prependToken token $ trans loc ch

-- mutual recursion with pushToken, push a token and continue scan with another scanner
prependToken :: Token -> Either ScanRes Scan -> Either ScanRes Scan
prependToken token (Left res) = Left $ (token :) <$> res
prependToken token (Right trans) = Right $ pushToken token trans

token :: String -> CodeLoc -> CodeLoc -> Type -> Token
token raw posL posR = Token raw rng where rng = CodeRng posL posR

nextLoc :: CodeLoc -> Char -> CodeLoc
nextLoc (CodeLoc ln col) '\n' = CodeLoc ln (col + 1)
nextLoc (CodeLoc ln col) _ = CodeLoc (ln + 1) col

quote :: String -> String
quote s = '"' : s ++ ['"']

append :: String -> Char -> String
append s ch = s ++ [ch]
