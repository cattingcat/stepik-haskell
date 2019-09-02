{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Sandbox.QualifiedConstr where

-- https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0109-quantified-constraints.rst


data Rose (f :: * -> *) (a :: *) = Rose (f a)

instance (Eq a, forall b. Eq b => Eq (f b)) => Eq (Rose f a) where
  (Rose x1) == (Rose x2) = x1 == x2