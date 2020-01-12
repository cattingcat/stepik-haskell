{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Sandbox.MyLensesComposition where
import Prelude hiding ((.), id)
import Control.Category (Category(..))
import Control.Monad.Identity (Identity(..))

data OldLens s a = OldLens
  { get    :: s -> a
  , modify :: (a -> a) -> s -> s }

(@.) :: forall a b c. OldLens b c -> OldLens a b -> OldLens a c
(@.) _c _b = OldLens get' modify'
  where
    get' :: a -> c
--    get' a = let b = get _b a in get _c b
    get' = get _c . get _b

    modify' :: (c -> c) -> a -> a
--    modify' f a = modify _b (\b -> modify _c f b) a
--    modify' f = modify _b (modify _c f)
    modify' = modify _b . modify _c


tupleLens :: OldLens (a, b) a
tupleLens = OldLens get' modify' where
  get' ~(a, b) = a
  modify' f ~(a, b) = (f a, b)

tst11 = modify tupleLens (modify tupleLens (+55)) ((1, 2), 3)
tst12 = modify (tupleLens @. tupleLens) (+55) ((1, 2), 3)

ix :: Int -> OldLens [a] a
ix n = OldLens (get' n) (modify' n) where
  get' 0 (x:_) = x
  get' n (x:xs) = get' (n - 1) xs
  get _ _ = error "Index err"
  modify' 0 f (x:xs) = f x : xs
  modify' n f (x:xs) = x : modify' (n - 1) f xs
  modify' _ _ _ = error "Inder err"

tst21 = modify (tupleLens @. ix 1) (+55) [(1, 2), (3, 4), (5, 6)]

{-

Category type-class describes structures with composition
class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

Examples:
  Kleisli arrow  = Monad m => a -> m b
  Fuctions       = (->) a b
  Lenses :)

-}

instance Category OldLens where
  id :: OldLens a a
  id = OldLens id (\f s -> f s)

  (.) :: OldLens b c -> OldLens a b -> OldLens a c
  (.) = (@.)

tst31 = modify (tupleLens . ix 1) (+55) [(1,2), (3,4), (5,6)]


-- New Lenses --
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a
-- (a -> f a) -> s -> f s

(@..) :: Lens' b c -> Lens' a b -> Lens' a c
--(@..) _bc _ab = \(f :: c -> f c) (s :: a) -> _ab (\b -> _bc f b) s
--(@..) _bc _ab = \f s -> _ab (\b -> _bc f b) s
--(@..) _bc _ab = \f -> _ab (\b -> _bc f b)
--(@..) _bc _ab = \f -> _ab (_bc f)
--(@..) _bc _ab = \f -> (_ab . _bc) f
--(@..) _bc _ab = _ab . _bc
(@..) = flip (.)

newtype Lenst s t a b = Lenst { runLenst :: Lens s t a b }
newtype Lenst' s a  = Lenst' { runLenst' :: Lens' s a }

instance Category Lenst' where
  id :: Lenst' a a
  id = Lenst' $ \f s -> f s

  (.) :: Lenst' b c -> Lenst' a b -> Lenst' a c
  (.) (Lenst' _bc) (Lenst' _ab) = Lenst' $ _bc @.. _ab

ixt :: Int -> Lenst' [a] a
ixt n = Lenst' $ lens n where
  lens :: Functor f => Int -> (a -> f a) -> [a] -> f [a]
  lens _ f [] = error "index error"
  lens 0 f (x:xs) = fmap (:xs) (f x)
  lens n f (x:xs) = fmap (x:) (lens (n - 1) f xs)

tst41 = runLenst' (ixt 5) (\i -> Identity (i + 1)) [1,2,3,4,5,6,7,8,9,0]

_1 :: Lenst' (a, b) a
_1 = Lenst' lens where
  lens f ~(a, b) = fmap (, b) (f a)
  
tst51 = runLenst' (_1 . ixt 1) (\i -> Identity (i + 1)) [(1,2), (3,4), (5,6)]

ixts :: Int -> Lenst' [a] a
ixts n = Lenst' $ lens n where
  lens :: Functor f => Int -> (a -> f a) -> [a] -> f [a]
  lens _ f [] = error "index error"
  lens 0 f (x:xs) = fmap (:xs) (f x)
  lens n f (x:xs) = fmap (x:) (lens (n - 1) f xs)