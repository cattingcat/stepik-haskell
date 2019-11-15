{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module ThinkingWithTypes.DependentPair where

import Data.Constraint
import Data.Maybe (mapMaybe)
import Data.Aeson
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Kind (Type)




data Sigma (f :: k -> Type) where
  Sigma :: Sing (a :: k) -> f a -> Sigma f

withSigma :: (forall (a :: k) . Sing a -> f a -> r) -> Sigma f -> r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma fa = Sigma sing fa

--fromSigma ::
--  forall k (a :: k) (f :: k -> Type) . (SingI a, SDecide k) =>
--  Sigma f -> Maybe (f a)
--fromSigma (Sigma s f) = case s %∼ sing @_ @a of
--  Proved Refl -> Just f
--  Disproved _ -> Nothing




abs :: Int -> Int
abs n | n > 0     = n
      | otherwise = -n


