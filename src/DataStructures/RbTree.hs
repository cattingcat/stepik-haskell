module DataStructures.RbTree where

data Color = R | B
data RbTree a = E | T Color a (RbTree a) (RbTree a)

--axioms:
--1. Red node cant have Red child
--2. Every path from root to leaf must have same amount of Black nodes

contains :: Ord a => RbTree a -> a -> Bool
contains E _ = False
contains (T _ a _ _) c | a == c = True
contains (T _ x l r) a = if x <= a then contains r a else contains l a

insert :: Ord a => a -> RbTree a -> RbTree a
insert a E = T R a E E
insert a t@(T color x l r)
  | x > a = balance color x l (insert a r)
  | x < a = balance color x (insert a l) r
  | otherwise = t

balance :: Ord a => Color -> a -> RbTree a -> RbTree a -> RbTree a
balance B v l@(T R v1 l1@(T R v2 l2 r2) r1) r = T R v1 (T B v2 l2 r2) (T B v r1 r)
balance B v l r = undefined



