{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module NaiveEval
  ( Value (..),
    eval,
  )
where
import qualified Parser as P

data Value = Nil | Bool Bool | Number Double | String String deriving (Eq)

instance Show Value where
  show Nil = "nil"
  show (Bool b) = show b
  show (Number n) = show n
  show (String s) = show s
  show _ = error "unsupported value"

unwrapBool :: Value -> Bool
unwrapBool (Bool b) = b
unwrapBool _ = error "not a boolean value"

unwrapNum :: Value -> Double
unwrapNum (Number n) = n
unwrapNum _ = error "not a number value"

unwrapString :: Value -> String
unwrapString (String s) = s
unwrapString _ = error "not a string value"

unwrapNil :: Value -> ()
unwrapNil Nil = ()
unwrapNil _ = error "not a nil value"

eval :: P.Expr -> Value
eval (P.LiteralExpr lit) = evalLiteral lit
eval (P.UnaryExpr uop sub) = evalUnary uop sub
eval (P.BinaryExpr bop lhs rhs) = evalBinary bop lhs rhs
eval _ = error "unsupported expression"

evalLiteral :: P.Literal -> Value
evalLiteral (P.Number n) = Number n
evalLiteral (P.String s) = String s
evalLiteral (P.Bool b) = Bool b
evalLiteral P.Nil = Nil
evalLiteral _ = error "unsupported literal"

evalUnary :: P.UnaryOp -> P.Expr -> Value
evalUnary op sub = let v = eval sub in opfunc op v
  where
    opfunc op = case op of
      P.Neg -> negf
      P.Not -> notf
      _ -> error "unsupported unary operator"
    negf v = Number (-unwrapNum v)
    notf v = Bool (not $ unwrapBool v)

evalBinary :: P.BinaryOp -> P.Expr -> P.Expr -> Value
evalBinary op lhs rhs =
  let lv = eval lhs; rv = eval rhs
   in opfunc op lv rv
  where
    opfunc op = case op of
      P.Eq -> eq
      P.Neq -> neq
      P.Lt -> lt
      P.Leq -> leq
      P.Gt -> gt
      P.Geq -> geq
      P.Plus -> plus
      P.Minus -> minus
      P.Times -> times
      P.Divides -> divides
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
