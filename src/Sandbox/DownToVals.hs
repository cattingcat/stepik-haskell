{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Sandbox.DownToVals where

data Alph = A | B deriving (Show)

data SomeType (a :: Alph) where 
    SomeTypeA :: SomeType A
    SomeTypeB :: SomeType B



newtype AlphW (a :: Alph) = AlphW Alph

class GetAlph (a :: Alph) where 
    get :: AlphW a

instance GetAlph A where 
    get = AlphW A
instance GetAlph B where 
    get = AlphW B

-- foo :: forall (a :: Alph) p. (GetAlph a) => p a -> Alph
getAlph :: forall (a :: Alph). (GetAlph a) => SomeType a -> Alph
getAlph a = case get :: AlphW a of 
    AlphW v -> v


tst1 :: Alph
tst1 = getAlph SomeTypeA

tst2 :: Alph
tst2 = getAlph SomeTypeB