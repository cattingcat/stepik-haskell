{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module ThinkingWithTypes.GeneralizeDepTypes where

import Data.Kind (Type)
import Data.Typeable
import Data.Void
import Unsafe.Coerce (unsafeCoerce)

data family Sign (a :: k)

data SomeSign k where 
  SomeSign :: Sign (a :: k) -> SomeSign k

-- | k used as a type and as a kind. See TypeInType ext  
withSomeSign :: SomeSign k -> (forall (a :: k) . Sign a -> r) -> r
withSomeSign (SomeSign a) f = f a

class SignKind k where
  type Demote k = r | r -> k
  toSign :: Demote k -> SomeSign k
  fromSign :: Sign (a :: k) -> Demote k

-- | Lift down type-level to value-level
class SignI (a :: k) where
  sign :: Sign a






-- | Bool example
data instance Sign (a :: Bool) where
  STrue  :: Sign 'True
  SFalse :: Sign 'False

instance SignKind Bool where
  type Demote Bool = Bool
  toSign True  = SomeSign STrue
  toSign False = SomeSign SFalse
  fromSign STrue  = True
  fromSign SFalse = False

instance SignI 'True where sign = STrue
instance SignI 'False where sign = SFalse



-- | Maybe example
data instance Sign (a :: Maybe k) where
  SNothing :: Sign 'Nothing
  SJust :: Sign (a :: k) -> Sign ('Just a)

instance (a ~ Demote a, SignKind a) => SignKind (Maybe a) where
  type Demote (Maybe a) = Maybe a
  toSign (Just a) = withSomeSign (toSign a) $ SomeSign . SJust
  toSign Nothing  = SomeSign SNothing
  fromSign (SJust a) = Just $ fromSign a
  fromSign SNothing  = Nothing

instance (SignI a) => SignI ('Just a) where sign = SJust sign
instance              SignI 'Nothing  where sign = SNothing



-- | List example
data instance Sign (a :: [k]) where
  SNil :: Sign '[]
  SCons :: Sign (a :: k) -> Sign (ts :: [k]) -> Sign (a ': ts)

instance (k ~ Demote k, SignKind k) =>  SignKind [k] where
  type Demote [k] = [k]
  toSign [] = SomeSign SNil
  toSign (a:as) = withSomeSign (toSign a) (\sh -> withSomeSign (toSign as) (SomeSign . SCons sh))
  fromSign SNil = []
  fromSign (SCons h t) = fromSign h : fromSign t
  
instance SignI '[] where sign = SNil
instance (SignI (a :: k), SignI (as :: [k])) => SignI (a ': as) where sign = SCons (sign @k @a) (sign @[k] @as)



tst1 = do
  c <- getChar
  case c of
    '0' -> let
      b = False
      someSign = toSign b
      in print $ withSomeSign someSign fromSign
    '1' -> let
          b = True
          someSign = toSign b
          in print $ withSomeSign someSign fromSign
    _ -> print "err"

-- Type application applies kinds firstly
tst2 = fromSign $ sign @Bool @'True



tst3Data :: Proxy '[True, False]
tst3Data = Proxy

tst3 :: forall ts . (SignI (ts :: [Bool])) => Proxy ts -> [Bool]
tst3 _ = fromSign (sign @[Bool] @ts) 



tst4Data :: Proxy ('Just 'True)
tst4Data = Proxy

tst4 :: (SignI (a :: k)) => Proxy a -> Sign a
tst4 _ = sign

-- Good example of type in type (k is a kind of a and return type)
tst5 :: forall k a . (k ~ Demote k, SignKind k, SignI (a :: k)) => Proxy a -> k
tst5 _ = fromSign @k (sign @k @a)
-- tst5 tst4Data 