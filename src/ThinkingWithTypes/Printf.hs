{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ThinkingWithTypes.Printf where

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits


data (a :: k1) :<< (b :: k2)
infixr 5 :<<

-- |  Associated type family  (type in typeclass)

class HasPrintf a where 
  type Printf a :: Type
  format :: String -> Proxy a -> Printf a
  
-- for all types with kind Symbol
instance KnownSymbol text => HasPrintf (text :: Symbol) where 
  type Printf text = String
  format acc _ = acc <> symbolVal (Proxy @text)
  
  
instance (KnownSymbol text, HasPrintf b) => HasPrintf ((text :: Symbol) :<< b) where 
  type Printf (text :<< b) = Printf b
  format acc _ = format (acc <> symbolVal (Proxy @text)) (Proxy @b)
  
instance (HasPrintf b, Show param) => HasPrintf ((param :: Type) :<< b) where 
  type Printf (param :<< b) = param -> Printf b
  format acc _ param = format (acc <> show param) (Proxy @b)
  
instance {-# OVERLAPPING #-} (HasPrintf b) => HasPrintf (String :<< b) where 
  type Printf (String :<< b) = String -> Printf b
  format acc _ s = format (acc <> s) (Proxy @b)  

-- | :kind! Printf(Int :<< ":" :<< Bool :<< "!")

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

tst1 = printf (Proxy @("kek " :<< Int :<< " puk" :<< String :<< "")) 666 "lolol"