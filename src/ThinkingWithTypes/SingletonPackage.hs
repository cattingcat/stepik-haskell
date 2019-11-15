{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module ThinkingWithTypes.SingletonPackage where

import Data.Singletons.Prelude
import Data.Singletons.TH

-- Generates Sing, SingKind, SingI instances
singletons [d|
    data TimeOfDay = Morning | Afternoon | Evening | Night deriving (Eq, Ord, Show)
  |]


tst :: forall k a . (k ~ Demote k, SingKind k, SingI (a :: k)) => Proxy a -> k
tst _ = fromSing @k (sing @a)

tst1 = tst $ Proxy @'Morning
