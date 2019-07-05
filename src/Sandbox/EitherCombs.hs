{-# LANGUAGE TypeOperators #-}

module HsFp2.MonadsAndEffects.EitherCombs where

data Error1 = A | B | C

data Error2 = D | E | F

data (|*|) a b = L a | R b

comb :: Either e1 a -> Either e2 b -> (a -> b -> c) -> Either (e1 |*| e2) c
comb (Right a) (Right b) f = Right (f a b)
comb (Left e1) _         f = Left (L e1)
comb _ (Left e2)         f = Left (R e2)