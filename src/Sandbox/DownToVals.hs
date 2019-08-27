{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-} -- we need it for (get :: AlphW a)


module Sandbox.DownToVals where

import GHC.TypeLits

data Alph = A | B deriving (Show)

data SomeType (a :: Alph) where 
    SomeTypeA :: SomeType A
    SomeTypeB :: SomeType B



newtype AlphW (a :: Alph) = AlphW { unAlph :: Alph }

class GetAlph (a :: Alph) where 
    get :: AlphW a

instance GetAlph A where 
    get = AlphW A
instance GetAlph B where 
    get = AlphW B

-- foo :: forall (a :: Alph) p. (GetAlph a) => p a -> Alph
getAlph :: forall (a :: Alph). (GetAlph a) => SomeType a -> Alph
getAlph a = unAlph (get :: AlphW a)


tst11 :: Alph
tst11 = getAlph SomeTypeA

tst12 :: Alph
tst12 = getAlph SomeTypeB



{-
    Same things for standart Strings type
    http://ponies.io/posts/2014-07-30-typelits.html
-}


data SomeSymboledType (a :: Symbol) = Ssta | Sstb

getSymb :: forall a. (KnownSymbol a) => SomeSymboledType a -> String
getSymb sst = symbolVal sst -- symbolVal also generalize proxy

tst21 :: String
tst21 = getSymb (Ssta :: SomeSymboledType "kek")