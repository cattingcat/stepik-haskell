{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ThinkingWithTypes.OpenSums where

import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce
import Fcf
import Control.Monad.Identity (Identity(..))

-- https://github.com/Lysxia/first-class-families

data OpenSum (f :: k -> Type) (ts :: [k]) where 
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts
  
type FindElem (key :: k) (ts :: [k]) = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member (t :: k) (ts :: [k]) = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts . Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval(FindElem t ts))

tst1 = findElem @"" @'["", "  "]

inj :: forall f t ts . Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts . Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum n ft) = if n == findElem @t @ts then Just (unsafeCoerce ft) else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 ft) = Left $ unsafeCoerce ft
decompose (UnsafeOpenSum n ft) = Right $ UnsafeOpenSum (n - 1) ft

weaken :: OpenSum f ts -> OpenSum f (t ': ts)
weaken (UnsafeOpenSum n ft) = UnsafeOpenSum (n + 1) ft

match :: forall f ts b . (forall t . f t -> b) -> OpenSum f ts -> b
match f (UnsafeOpenSum _ ft) = f ft


tstData :: OpenSum Identity '[Int, String]
tstData = inj (Identity (55::Int))



-- :kind! Eval (FindElem Int '[Int, String, Double])         = 0