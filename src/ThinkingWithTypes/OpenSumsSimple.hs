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

module ThinkingWithTypes.OpenSumsSimple where

import Data.Kind (Type, Constraint)
import Data.Proxy
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce
import Fcf
import Control.Monad.Identity (Identity(..))


data OpenSum (ts :: [Type]) where
  UnsafeOpenSum :: Int -> t -> OpenSum ts

type FindElem (key :: k) (ts :: [k]) = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member (t :: k) (ts :: [k]) = KnownNat (Eval (FindElem t ts))

index :: forall t ts . Member t ts => Int
index = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

ins :: forall t ts . Member t ts => t -> OpenSum ts
ins = UnsafeOpenSum (index @t @ts)

prj :: forall t ts . Member t ts => OpenSum ts -> Maybe t
prj (UnsafeOpenSum n a) = if n == index @t @ts then Just $ unsafeCoerce a else Nothing


-- data can be curried (type - synonyms and data families can't)

--data CombineConstr :: Constraint -> Constraint -> Exp Constraint
--type instance Eval (CombineConstr c1 c2) = (c1, c2)
--
--type family Unit :: k where
--  Unit = (() :: Constraint)
--
--type AllHas (c :: k -> Constraint) (ts :: [k]) = Eval (Foldr CombineConstr Unit (Eval (Map (Pure1 c) ts)))
--
--instance (Show t, AllHas Show ts) => Show (OpenSum (t ': ts)) where
--  show (UnsafeOpenSum n a) = show (UnsafeOpenSum (n - 1) a :: OpenSum ts)
--
--instance Show (OpenSum '[]) where
--  show _ = "empty"

tst1 :: OpenSum '[Int, String, Double]
tst1 = ins (55::Int)

tst2 :: Maybe Int
tst2 = prj tst1

tst3 :: Maybe Double
tst3 = prj tst1












