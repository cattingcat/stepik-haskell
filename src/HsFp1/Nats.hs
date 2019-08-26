module HsFp1.Nats where

data Nat = Zero | Suc Nat deriving (Show)

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

add :: Nat -> Nat -> Nat
add Zero b = b
add (Suc a) b = Suc $ add a b

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Suc Zero) b = b
mul (Suc a) b = add b (mul a b)

fac :: Nat -> Nat
fac Zero = Zero
fac (Suc Zero) = (Suc Zero)
fac as@(Suc a) = mul as (fac a)




data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf _) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node a b) = 1 + size a + size b

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node a b) = let
        (ca,sa) = go a
        (cb,sb) = go b in (ca+cb, sa+sb)