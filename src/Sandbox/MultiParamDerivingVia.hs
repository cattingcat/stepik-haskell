{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

-- | Paper:
-- | https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf

module Sandbox.MultiParamDerivingVia where

import Prelude hiding ((!), (!!))
import Data.Constraint (Constraint)
import Data.Kind (Type)

class WeakVector v a where
  (!) :: v a -> Int -> Maybe a
  
class WeakVectorFlip a v where
  (!!) :: v a -> Int -> Maybe a


instance WeakVector [] a where
  (!) (a:_) 0 = Just a
  (!) [] _    = Nothing
  (!) (_:as) n = as ! (n - 1)

instance WeakVectorFlip a [] where
  (!!) (a:_) 0 = Just a
  (!!) [] _    = Nothing
  (!!) (_:as) n = as ! (n - 1)


newtype NList s a = NList [a]

deriving via [] instance (WeakVectorFlip a (NList String))

-- | Incorrect order
-- deriving via [] instance (WeakVector (NList s) a)

-- | Impossible to use type families
-- deriving via [] instance (Flip WeakVector a (NList s))

tstNList :: NList String Int
tstNList = NList [1,2,3,4,5,6,7]

tst3 = tstNList !! 3
tst4 = tstNList !! 33

type family Flip (f :: k1 -> k2 -> Constraint) (a :: k2) (b :: k1) :: Constraint where  
  Flip f a b = f b a