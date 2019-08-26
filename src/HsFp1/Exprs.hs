module HsFp1.Exprs where

infixl 6 :+:
infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand ((e1 :+: e2) :*: e) =
  expand (expand e1 :*: expand e) :+:
  expand (expand e2 :*: expand e)
expand (e :*: (e1 :+: e2)) =
  expand (expand e :*: expand e1) :+:
  expand (expand e :*: expand e2)
expand (e1 :+: e2) = expand e1 :+: expand e2
expand (e1 :*: e2) = let
    ee1 = expand e1
    ee2 = expand e2
  in if ee1 == e1 && ee2 == e2 then e1 :*: e2 else expand $ ee1 :*: ee2
expand e = e


test1 = expand $ (Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5)

