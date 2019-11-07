{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ThinkingWithTypes.ThinkingWithTypes2 where

import GHC.Base
import Data.Typeable (Typeable, cast)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
import qualified Data.Monoid as M

-- | Isomorphisms:
-- |   Identity a   ~   a   ~   forall r . (a -> r) -> r

newtype Cont a = Cont { unCont :: forall r . (a -> r) -> r }

runCont :: Cont a -> a
runCont (Cont f) = f id

instance Functor Cont where
  fmap f (Cont c) = Cont $ \cb -> c (cb . f)

instance Applicative Cont where
  pure a = Cont $ \cb -> cb a
  liftA2 f (Cont fa) (Cont fb) = Cont $ \cbc -> fa (\a -> fb (\b -> cbc (f a b)))

instance Monad Cont where
  return = pure
  (>>=) c f = joinCont (fmap f c)
--  (>>=) (Cont ma) f = Cont $ \cb -> ma (\a -> unCont (f a) cb)

joinCont :: Cont (Cont a) -> Cont a
joinCont (Cont mc) = Cont $ \cb -> mc (\c -> unCont c cb)


newtype ContT m a = ContT { unContT :: forall r . (a -> m r) -> m r }

instance Functor (ContT m) where
  fmap f (ContT c) = ContT $ \cb -> c (\a -> cb (f a))

instance Applicative (ContT m) where
  pure a = ContT $ \cb -> cb a
  liftA2 f (ContT fa) (ContT fb) = ContT $ \cbc -> fa (\a -> fb (\b -> cbc (f a b)))

instance Monad (ContT m) where
  return = pure
  (>>=) (ContT c) f = ContT $ \cb -> c (\a -> unContT (f a) cb)



-- | existential types

data HasShow = forall a . Show a => MkHasShow a

foo :: HasShow -> String
foo (MkHasShow a) = show a

tst1 = foo $ MkHasShow "kek"
tst2 = foo $ MkHasShow 44
tst3 = foo $ MkHasShow (Just "puk")

data MyAny where
  MyAny :: a -> MyAny

elimAny :: (forall a . a -> r) -> MyAny -> r
elimAny f (MyAny a) = f a

elimSomeData :: (forall a . Show a => a -> r) -> HasShow -> r
elimSomeData f (MkHasShow a) = f a

-- 7.1_iii
instance Show HasShow where
  show = elimSomeData show
  
  
  
data Dynamic = forall a . Typeable a => MkDynamic a

elimDynamic :: (forall a . Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (MkDynamic a) = f a

dynamicCast :: Typeable a => Dynamic -> Maybe a
dynamicCast = elimDynamic cast

liftD2 :: forall a b c . (Typeable a, Typeable b, Typeable c) =>
  Dynamic -> Dynamic -> (a -> b -> c) -> Maybe Dynamic
liftD2 d1 d2 f = MkDynamic <$> liftA2 f (dynamicCast @a d1) (dynamicCast @b d2)


plus :: Dynamic -> Dynamic -> Dynamic
plus a b = fromMaybe (error "wrong types") $ asum 
  [
    liftD2 @String @String a b (++),
    liftD2 @Int @Int a b (+),
    liftD2 @String @Int a b (\x y -> x ++ (show y)),
    liftD2 @Int @String a b (\x y -> show x ++ y)
  ]
  
tst4 = dynamicCast @String $ (MkDynamic (5::Int)) `plus` (MkDynamic "kek")


-- | Generalization (ConstraintKinds ext)

data Has (c :: Type -> Constraint) where 
  Has :: c t => t -> Has c
  
elimHas :: (forall a . c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasShow2 = Has Show
type Dynamic2 = Has Typeable

--  It doesn't work
--type MonoidAndEq a = (Monoid a, Eq a)
--type HasMonoidAndEq = Has MonoidAndEq

class    (Monoid a, Eq a) => MonoidAndEq a
instance (Monoid a, Eq a) => MonoidAndEq a

type HasMonoidAndEq = Has MonoidAndEq

tst5a :: HasMonoidAndEq
tst5a = Has (M.Sum (0::Int))

tst5 = elimHas (\a -> a <> a == a) tst5a