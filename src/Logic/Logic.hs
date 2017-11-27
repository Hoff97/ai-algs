module Logic.Logic where

data Term a = Constant a | Variable String | Function String [Term a] deriving Eq

data Operand = And | Or | Xor | Then | Iff deriving Eq

data Expr a = TExpr (Term a)
  | Predicate [Expr a]
  | All String (Term a)
  | Exists String (Term a)
  | Operation (Expr a) Operand (Expr a)
  | Negation (Expr a) deriving Eq

(&&) :: Expr a -> Expr a -> Expr a
a && b = Operation a And b

(||) :: Expr a -> Expr a -> Expr a
a || b = Operation a Or b

(><) :: Expr a -> Expr a -> Expr a
a >< b = Operation a Xor b

(|>) :: Expr a -> Expr a -> Expr a
a |> b = Operation a Then b

(<|>) :: Expr a -> Expr a -> Expr a
a <|> b = Operation a Iff b
