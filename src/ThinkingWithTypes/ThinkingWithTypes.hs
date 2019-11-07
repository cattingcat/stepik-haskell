{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}

module ThinkingWithTypes.ThinkingWithTypes where

import Control.Monad.Trans (MonadTrans)
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality (type (==))
import Data.Typeable (Typeable)
import Data.Data (Proxy(..), typeRep)
import Data.Kind(Constraint, Type)

-- (a * b)^c = a^c * b^c
ex_1_4_ii :: (c -> (a, b)) -> (c -> a, c -> b)
ex_1_4_ii f = (fst . f, snd . f)


-- (a^b)^c = a^(b*c)
ex_1_4_iii :: (c -> (b -> a)) -> (b, c) -> a
ex_1_4_iii f (b, c) = f c b

ex_1_4_iii' :: (c -> b -> a) -> (c, b) -> a
ex_1_4_iii' = uncurry

-- ex_2_1_3_i:   Show :: Type -> Constraint
-- ex_2_1_3_ii:  Functor :: (Type -> Type) -> Constraint
-- ex_2_1_3_iii:  Monad :: (Type -> Type) -> Constraint
-- ex_2_1_3_iiii:  MonadTrans :: (Type -> Type) -> Type -> Constraint

data Dt1 (a :: Symbol) = MkDt1
foo1 :: Dt1 a -> Dt1 b -> Dt1 (AppendSymbol a b)
foo1 _ _ = MkDt1

foo1tst = foo1 (MkDt1 :: Dt1 "kek ") (MkDt1 :: Dt1 "puk")

foo2 :: Dt1 a -> Dt1 b -> Dt1 (If (CmpSymbol a b == GT) a b)
foo2 _ _ = MkDt1

foo2tst = foo2 (MkDt1 :: Dt1 "zkek ") (MkDt1 :: Dt1 "apuk")


--  :kind  [Bool]     :: Type
--  :kind '[Bool]     :: [Type]
--  :kind '[ 'True ]  :: [Bool]


-- ex_2_4_i
type family MyNot (a :: Bool) :: Bool where
  MyNot 'True = 'False
  MyNot 'False = 'True


-- ex_3_i
-- see contravariant and invariant packages
newtype T1 a = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 a) = T1 $ f . a

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 a) = T5 $ \cb -> a (cb . f)


-- ScopedType vars, but you have to explicitly show "forall" type vars
--  forall makes new type-var scope
broken :: forall a b . (a -> b) -> a -> b
broken f a = apply where
  apply :: b
  apply = f a


-- TypeApplication
foo3 = fmap @Maybe
--foo4 = fmap @_ @Int @String

-- AllowAmbiguousTypes
--  we don't have a at right side of sig
--  so we put type variable via TypeApplication
typeName :: forall a . Typeable a => String
typeName = (show . typeRep) (Proxy @a)

tst5 = typeName @Int



data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLen :: HList ts -> Int
hLen HNil = 0
hLen (h :# t) = 1 + hLen t

hHead :: HList (t ': ts) -> t
hHead (h :# _) = h

show3of4 :: Show a => HList '[_1, _2, a, _3] -> String
show3of4 (_ :# _ :# a :# _) = show a

tst6' = 1 :# 2.9 :# "kek" :# Just True :# HNil
tst6 = show3of4 tst6'


-- | Old style of instance declaration. See below with type family

--instance Eq (HList '[]) where
--  (==) _ _ = True
--
--instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
--  (==) (x:#xs) (y:#ys) = x == y && xs == ys
--
--instance Ord (HList '[]) where
--  compare _ _ = EQ
--
--instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
--  compare (x:#xs) (y:#ys) = let c = compare x y in if c == EQ then compare xs ys else c
--
--
--instance Show (HList '[]) where
--  show _ = "HNil"
--
--instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
--  show (x:#xs) = show x ++ " :# " ++ show xs

tst7 = show tst6'


type family AllEq (ts :: [Type]) :: Constraint where
  AllEq '[]       = ()
  AllEq (t ': ts) = (Eq t, AllEq ts)

-- :kind! AllEq '[Int, Bool]

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All _ '[]       = ()
  All c (t ': ts) = (c t, All c ts)


instance All Eq ts => Eq (HList ts) where
  (==) HNil HNil = True
  (==) (x:#xs) (y:#ys) = x == y && xs == ys

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
  compare HNil HNil = EQ
  compare (x:#xs) (y:#ys) = let c = compare x y in if c == EQ then compare xs ys else c

instance All Show ts => Show (HList ts) where
  show HNil = "HNil"
  show (x:#xs) = show x ++ " :# " ++ show xs


-- Rank2Types/RankNTypes ext
--  id - rank 1, because only one level of polymorphic params
--  applyToFive - rank 2, because it receives rank-1 function a -> a
applyToFive :: (forall a . a -> a) -> Int
applyToFive f = f 5

tstRankFoo :: (forall a . a -> (forall b . b -> b) -> a) -> Int
tstRankFoo f = f 5 id

tst8 = applyToFive id