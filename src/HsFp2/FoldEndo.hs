{-# LANGUAGE TypeOperators #-}

module HsFp2.FoldEndo where
import Data.Monoid

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = Endo . (foldr (.) id)

test11 = Product 2 <> Product 3 -- mappend synonym


-- foldMap :: Monoid m, Foldable t => (a -> m) -> t a -> m
-- foldr f ini cont = (foldMap (Endo . f) cont) ini


-- Endo :: a -> a
-- Dual :: Monoid a => Monoid (Dual a) - change arg order in mappend


infixr 9 |.|

newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldr f ini (Cmps fga) = foldr (\a b -> foldr f b a) ini fga


test21 = maximum $ Cmps [Nothing, Just 2, Just 3]
test22 = length $ Cmps [[1,2], [], [3,4,5,6,7]]