{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module ThinkingWithTypes.GenericEmptyCtor where

import GHC.Generics
import Debug.Trace (trace)
import GHC.Base (Type)

-- | returns value which was constructed by single empty ctor
--    for example 
--    a = Maybe   => Just Nothing
--    a = (a,b)   => Nothing
exNihilo :: forall a . (Generic a, GGetEmptyCtor (Rep a)) => Maybe a
exNihilo = case getEmptyCtor of 
  Just ctor -> Just $ to ctor
  Nothing   -> Nothing


class GGetEmptyCtor (a :: Type -> Type) where 
  getEmptyCtor :: Maybe (a x)


-- skip meta info
instance GGetEmptyCtor a => GGetEmptyCtor (M1 _x _y a) where 
  getEmptyCtor = case getEmptyCtor @a of 
    Just ctor -> Just $ M1 ctor
    Nothing   -> Nothing

-- represents empty ctor
instance GGetEmptyCtor U1 where 
  getEmptyCtor = Just U1

-- data types without ctors
instance GGetEmptyCtor V1 where 
  getEmptyCtor = Nothing

-- constants??
instance GGetEmptyCtor (K1 _1 a) where 
  getEmptyCtor = Nothing

-- Sum types
instance (GGetEmptyCtor a, GGetEmptyCtor b) => GGetEmptyCtor (a :+: b) where 
  getEmptyCtor = case (getEmptyCtor @a, getEmptyCtor @b) of 
    (Just ca, Nothing) -> Just $ L1 ca
    (Nothing, Just cb) -> Just $ R1 cb
    _                  -> Nothing

-- Product types
instance GGetEmptyCtor (a :*: b) where 
  getEmptyCtor = Nothing





data Foo a b = F1 | F2 a | F3 a b deriving (Generic, Show)

tst1 :: Maybe (Foo String Int)
tst1 = exNihilo







