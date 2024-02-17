module Scanner (Type (..), CodeLoc (..), CodeRng (..), Token (..), ScanErr (..), scan) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Maybe (fromMaybe, listToMaybe)
import Debug.Trace

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
  | NOT
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

char1symbols :: [(String, Type)]
char1symbols =
  [ ("(", LEFT_PAREN),
    (")", RIGHT_PAREN),
    ("{", LEFT_BRACE),
    ("}", RIGHT_BRACE),
    (",", COMMA),
    (".", DOT),
    ("-", MINUS),
    ("+", PLUS),
    (";", SEMICOLON),
    ("/", SLASH),
    ("*", STAR),
    ("!", BANG),
    ("<", LESS),
    (">", GREATER)
  ]

char2symbols :: [(String, Type)]
char2symbols =
  [ ("!=", BANG_EQUAL),
    ("==", EQUAL_EQUAL),
    ("<=", LESS_EQUAL),
    (">=", GREATER_EQUAL)
  ]

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
    ("not", NOT),
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
  deriving (Eq, Ord)

instance Show CodeLoc where
  show (CodeLoc ln col) = show ln ++ ":" ++ show col

data CodeRng = CodeRng CodeLoc CodeLoc
  deriving (Eq, Ord)

instance Show CodeRng where
  show (CodeRng l r) = "[" ++ show l ++ " to " ++ show r ++ "]"

data Token = Token {tokRaw :: String, tokRng :: CodeRng, tokType :: Type}
  deriving (Show, Eq, Ord)

token :: String -> CodeLoc -> CodeLoc -> Type -> Token
token raw l r = Token raw (CodeRng l r)

newtype ScanErr = ScanErr {reason :: String}
  deriving (Show, Eq, Ord)

scan :: String -> (Either ScanErr (), [Token])
scan = flip scan' (CodeLoc 1 1)

scan' str loc = case scanOnce loc str of
  (Left (ScanErr e), (_, errloc)) -> (Left $ ScanErr $ e ++ " at " ++ show errloc, [])
  (Right token, (rest, loc')) ->
    if tokType token == EOF
      then (Right (), [token])
      else (token :) <$> scan' rest loc'

scanOnce, scanStr, scanNum, scanIdkw, skipCmtScan :: CodeLoc -> String -> (Either ScanErr Token, (String, CodeLoc))
scanOnce loc@(CodeLoc ln col) src
  | null src = (Right $ token "" loc loc EOF, ("", loc))
  | ch == '"' = scanStr loc src
  | isDigit ch = scanNum loc src
  | isAlpha ch = scanIdkw loc src
  | isSpace ch && ch /= '\n' = scanOnce loc' suf1
  | ch == '\n' = scanOnce locnl suf1
  | pre2 == "//" = skipCmtScan loc'' suf2
  | Just symbol2 <- lookup pre2 char2symbols = (Right $ token pre2 loc loc' symbol2, (suf2, loc''))
  | Just symbol1 <- lookup pre1 char1symbols = (Right $ token pre1 loc loc symbol1, (suf1, loc'))
  | otherwise = (Left $ ScanErr "unexpected char", (src, loc))
  where
    (pre1, suf1) = splitAt 1 src
    (pre2, suf2) = splitAt 2 src
    loc' = CodeLoc ln (col + 1)
    loc'' = CodeLoc ln (col + 2)
    locnl = CodeLoc (ln + 1) 1
    ch = head src

-- | NOTE: the original lexical grammar of lox does not support escape sequence
scanStr loc0@(CodeLoc ln col) ('"' : rest) = scanStr' (CodeLoc ln (col + 1)) rest ['"']
  where
    scanStr' :: CodeLoc -> String -> String -> (Either ScanErr Token, (String, CodeLoc))
    scanStr' loc@(CodeLoc ln col) src acc =
      let loc' = CodeLoc ln (col + 1)
          loc'' = CodeLoc ln (col + 2)
       in case src of
            "" -> (Left $ ScanErr "unexpected EOF", (src, loc))
            ('"' : rest) -> (Right $ token (acc ++ ['"']) loc0 loc STRING, (rest, loc'))
            ('\\' : '"' : rest) -> scanStr' loc'' rest (acc ++ "\\\"")
            ('\\' : '\\' : rest) -> scanStr' loc'' rest (acc ++ "\\\\")
            ('\\' : 'b' : rest) -> scanStr' loc'' rest (acc ++ "\\b")
            ('\\' : 'f' : rest) -> scanStr' loc'' rest (acc ++ "\\f")
            ('\\' : 't' : rest) -> scanStr' loc'' rest (acc ++ "\\t")
            ('\\' : 'r' : rest) -> scanStr' loc'' rest (acc ++ "\\r")
            ('\\' : 'n' : rest) -> scanStr' loc'' rest (acc ++ "\\n")
            ('\\' : ch : rest) -> (Left $ ScanErr $ "cannot escape " ++ [ch], (rest, loc'))
            (ch : rest) -> scanStr' (nextLoc ch loc) rest (acc ++ [ch])

scanNum loc@(CodeLoc ln col) src = (Right $ token raw loc loc' NUMBER, (rest', loc''))
  where
    (intPart, rest) = span isDigit src
    (decPart, rest') =
      if listToMaybe rest == Just '.'
        then ('.' :) `first` span isDigit (tail rest)
        else ("", rest)
    raw = if null decPart then intPart else intPart ++ decPart
    loc' = CodeLoc ln (col + length raw - 1)
    loc'' = CodeLoc ln (col + length raw)

scanIdkw loc@(CodeLoc ln col) src = (Right $ token raw loc loc' toktp, (rest, loc''))
  where
    (raw, rest) = span isAlphaNum src
    loc' = CodeLoc ln (col + length raw - 1)
    loc'' = CodeLoc ln (col + length raw)
    toktp = kwidType raw

skipCmtScan loc@(CodeLoc ln col) src = scanOnce loc' rest
  where
    (raw, rest) = span (/= '\n') src
    loc' = CodeLoc ln (col + length raw)

nextLoc :: Char -> CodeLoc -> CodeLoc
nextLoc ch (CodeLoc ln col) = if ch == '\n' then CodeLoc (ln + 1) 1 else CodeLoc ln (col + 1)
