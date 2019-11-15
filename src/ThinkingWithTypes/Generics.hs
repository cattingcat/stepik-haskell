{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module ThinkingWithTypes.Generics where

import GHC.Generics



-- | Generic typeclass, -XDeriveGeneric ext
--    :kind! BoolGen
--    :kind! TupleGen

type BoolGen = Rep Bool
type TupleGen = Rep (String, Int)



-- | Generic Eq example

class GEq a where 
  geq :: a x -> a x -> Bool
  
-- represents empty ctor
instance GEq U1 where 
  geq U1 U1 = True
  
-- data types without ctors
instance GEq V1 where 
  geq _ _ = True
  
-- constants??
instance Eq a => GEq (K1 _1 a) where 
  geq (K1 a) (K1 b) = a == b
  
-- Sum types
instance (GEq a, GEq b) => GEq (a :+: b) where 
  geq (L1 a) (L1 b) = geq a b
  geq (R1 a) (R1 b) = geq a b
  geq _      _      = False

-- Product types
instance (GEq a, GEq b) => GEq (a :*: b) where 
  geq (a1 :*: b1) (a2 :*: b2) = geq a1 a1 && geq b1 b2
  
-- D1 C1 S1 are synonyms of M1
--  metainfo
instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2
  
genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = from a `geq` from b

tst1 = genericEq True True
tst2 = genericEq (5, 6) (5, 6)


data Foo a b c = F0 | F1 a | F2 b c  deriving (Generic)

-- we have to add Eq constraints for args
instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
  (==) = genericEq
  
-- default instance (DefaultSignatures)
class MyEq a where 
  eq :: a -> a -> Bool
  
  default eq :: 
    (Generic a, GEq (Rep a)) => 
    a -> a -> Bool
  eq a b = from a `geq` from b
  
-- DeriveAnyClasses allows to derive instances with defaulc sig
data Foo1 a b c = F10 | F11 a | F12 b c  deriving (Generic, MyEq)