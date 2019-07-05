module Functors where

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf a) = Leaf $ f <$> a 
    fmap f (Branch l v r) = Branch (fmap f l) (f <$> v) (fmap f r)