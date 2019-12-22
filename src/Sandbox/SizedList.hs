{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Sandbox.SizedList where
import Prelude hiding (concat, (+))
import GHC.TypeLits
import Data.Type.Equality ((:~:) (..))


data PeanoNum = Z | S PeanoNum

type family (n :: PeanoNum) |+| (m :: PeanoNum) :: PeanoNum where
  Z |+| m = m
  S n |+| m = S (n |+| m)

type family Peano (n :: Nat) :: PeanoNum where
  Peano 0 = 'Z
  Peano n = 'S (Peano (n - 1))

data List (n :: PeanoNum) a where
  Nil :: List 'Z a
  Cons :: a -> List n a -> List ('S n) a

infixr 5 |:
(|:) = Cons

concat :: List n a -> List m a -> List (n |+| m) a
concat Nil m = m
concat (Cons a l) m = Cons a $ concat l m

type SList (n :: Nat) = List (Peano n) 

first :: SList 5 Int -> Int
first (Cons n _) = n

third :: SList 5 Int -> Int
third (Cons _ (Cons _ (Cons a _))) = a

tst1 = first (1 |: 2 |: 3 |: 4 |: 5 |: Nil)
tst2 = first $ (1 |: 2 |: 3 |: Nil) `concat` (4 |: 5 |: Nil)
tst3 = third $ (1 |: 2 |: 3 |: Nil) `concat` (4 |: 5 |: Nil)
--tst4 = third $ (1 |: 2 |: 3 |: Nil) `concat` (4 |: 5 |: 6 |: Nil)






type Arity n = (KnownNat n, Peano (n + 1) ~ 'S (Peano n))

lemma1 :: List 'Z a -> List m a -> List ('Z |+| m) a :~: List m a
lemma1 Nil m = Refl

lemma2 :: List ('S n) a -> List m a -> List (n |+| 'S m) a :~: List ('S (n |+| m)) a
lemma2 (Cons _ rest) m = case rest of
  Nil      -> case lemma1 rest m of Refl -> Refl
  Cons _ _ -> case lemma2 rest m of Refl -> Refl