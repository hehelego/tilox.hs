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

newtype Prog = Prog [Decl]

data Decl
  = StmtDecl Stmt
  | VarDecl Ident (Maybe Expr)

data Stmt
  = ExprStmt Expr
  | PrintStmt Expr

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

data BinaryOp = Eq | Neq | Lt | Leq | Gt | Geq | Plus | Minus | Times | Divides

newtype Ident = Ident String deriving (Eq)

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
