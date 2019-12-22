{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ThinkingWithTypes.Printf where

import Prelude hiding ((:), (+))
import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits hiding ((+))


data (a :: k1) # (b :: k2)
infixr 5 #

class HasPrintf a where 
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a


instance KnownSymbol text => HasPrintf (text :: Symbol) where 
  type Printf text = String
  format acc _ = acc <> symbolVal (Proxy @text)

instance (KnownSymbol text, HasPrintf b) => HasPrintf ((text :: Symbol) # b) where
  type Printf (text # b) = Printf b
  format acc _ = format (acc <> symbolVal (Proxy @text)) (Proxy @b)
  
instance (HasPrintf b, Show param) => HasPrintf ((param :: Type) # b) where
  type Printf (param # b) = param -> Printf b
  format acc _ param = format (acc <> show param) (Proxy @b)
  
instance {-# OVERLAPPING #-} (HasPrintf b) => HasPrintf (String # b) where
  type Printf (String # b) = String -> Printf b
  format acc _ s = format (acc <> s) (Proxy @b)


printf :: forall a . HasPrintf a => Printf a
printf = format "" (Proxy @a)

tst1 = printf @("Hello, my name is " # String # " and I love Dart since ver " # Int # "") "Mark" 2
--tst2 = printf @("Hello, my name is " # String # " and I love Dart since ver " # Int # "") "Mark" 2.2

tst3 = printf @("Hello, my name is " # String # " and I love Dart since ver " # Int # "")