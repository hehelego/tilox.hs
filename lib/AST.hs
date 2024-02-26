{-# LANGUAGE GADTs #-}

module AST
  ( Expr (..),
    Literal (..),
    UnaryOp (..),
    BinaryOp (..),
    Prog (..),
    Decl (..),
    Stmt (..),
    Ident (..),
  )
where

import Data.List (intercalate)

newtype Prog = Prog [Decl]

data Decl
  = StmtDecl Stmt
  | VarDecl Ident (Maybe Expr)

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr
  | BlockStmt [Decl]
  | IfStmt Expr Stmt Stmt -- if (cond) branch else branch
  | ForStmt Decl Expr Expr Stmt -- for(init-decl;cond-expr;next-expr) body
  | WhileStmt Expr Stmt -- while(condition) body
  | EmptyStmt

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
  | AsgnExpr Ident Expr

data Literal = Ref Ident | Number Double | String String | Bool Bool | Nil

data UnaryOp = Neg | Not

data BinaryOp = Eq | Neq | Lt | Leq | Gt | Geq | Plus | Minus | Times | Divides | And | Or

newtype Ident = Ident String deriving (Eq)

instance Show Prog where
  show (Prog decls) = show $ BlockStmt decls

instance Show Decl where
  show (StmtDecl stmt) = show stmt
  show (VarDecl var init) = "var " ++ show var ++ initval ++ ";"
    where
      initval = case init of
        Just expr -> " = " ++ show expr
        Nothing -> ""

instance Show Stmt where
  show (ExprStmt e) = show e ++ ";"
  show (PrintStmt e) = "print " ++ show e ++ ";"
  show (IfStmt cond t f) = unlines [part1, part2, part3, part4]
    where
      part1 = "if (" ++ show cond ++ ")"
      part2 = pad $ show t
      part3 = "else"
      part4 = pad $ show f
  show (ForStmt init cond next body) = unlines [part1, part2]
    where
      part1 = unwords ["for", "(", show init, show cond, ";", show next, ")"]
      part2 = pad $ show body
  show (WhileStmt cond body) = unlines [part1, part2]
    where
      part1 = unwords ["while", "(", show cond, ")"]
      part2 = pad $ show body
  show (BlockStmt ss) = "{\n" ++ inner ++ "\n}"
    where
      inner = pad $ intercalate "\n" $ show <$> ss
  show EmptyStmt = ";"

pad :: String -> String
pad = unlines . fmap ('\t' :) . lines

instance Show Ident where
  show (Ident id) = id

instance Show Expr where
  show (LiteralExpr lit) = show lit
  show (UnaryExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
  show (BinaryExpr op l r) = "(" ++ show l ++ " " ++ show op ++ " " ++ show r ++ ")"
  show (AsgnExpr id e) = show id ++ " = " ++ show e

instance Show Literal where
  show (Ref id) = show id
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
  show And = "and"
  show Or = "or"
