{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Sandbox.IoTests where
import Data.Foldable hiding (foldMap')
import Data.Monoid
import Control.Monad
import Control.Monad.Reader

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' = undefined

foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' f z ta = appEndo (foldMap' mapf ta) z where
    mapf a = Endo (\x -> f x a)

data Kek a where 
    Puk :: Kek a
    Lol :: a -> Kek a

tst :: Maybe Int
tst = Just 5

-- type equivalence
-- https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/equality-constraints.html
foo :: a ~ (Kek Int) => a -> a
foo = undefined