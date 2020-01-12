{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module ExtTest.TypeclassExts where

class Iso a b where 
  fw :: a -> b
  bw :: b -> a
  
instance Iso ((), a) a where 
  fw (_, a) = a
  bw a      = ((), a) 
  
--instance Iso a a where 
--  fw = id
--  bw = id
  
instance Iso a (a, ()) where 
  fw a      = (a, ()) 
  bw (a, _) = a
  
instance (Iso a b, Iso b c) => Iso a c where 
  fw = fw @b @c . fw @a @b
  bw = bw @a @b . bw @b @c
  
-- | doesnt work:
--tst :: ((), Int) -> (Int, ())
--tst = fw


data Isomorphism a b = Isomorphism {
    _fw :: a -> b,
    _bw :: b -> a 
}
  
  
isoTupleToA :: Isomorphism ((), a) a
isoTupleToA = Isomorphism snd ((),)

isoAtoTuple :: Isomorphism a (a, ())
isoAtoTuple = Isomorphism (,()) fst

composeIso :: Isomorphism a b -> Isomorphism b c -> Isomorphism a c
composeIso (Isomorphism a2b b2a) (Isomorphism b2c c2b) = Isomorphism (b2c . a2b) (b2a . c2b)

tst2 :: Isomorphism ((), Int) (Int, ()) -> ((), Int) -> (Int, ())
tst2 (Isomorphism a2b _) = a2b



-- | And more with rank-n-types
data MonadI m = MonadI {
    _return :: forall a . a -> m a,
    _bind   :: forall a b . m a -> (a -> m b) -> m b 
}