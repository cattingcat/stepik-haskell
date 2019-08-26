{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Sandbox.SingletonTypes where

import GHC.TypeLits

data family Sing (a :: k)
data instance Sing (a :: Bool) where
    STrue :: Sing True
    SFalse :: Sing False

type SBool (k :: Bool) = Sing k

sBoolToBool :: SBool b -> Bool
sBoolToBool STrue = True
sBoolToBool _     = False

data MyMaybe a = MyJust a | MyNothing

type family IsEmpty (a :: MyMaybe b) :: Bool
type instance IsEmpty (MyJust a) = False
type instance IsEmpty MyNothing = True

newtype ExType a b = ExType b

isEmptyTstFoo :: ExType True b -> b
isEmptyTstFoo (ExType b) = b

kek :: ExType (IsEmpty (MyJust Int)) Int 
kek = ExType 55

-- puk :: ExType b Int -> Sing b -> Bool
-- puk (ExType v) STrue = True
-- puk (ExType v) SFalse = False

-- tst1 = puk kek SFalse

-- puk :: Sing b -> Bool
-- puk STrue = True
-- puk SFalse = False


{- Default type family, can be extended
type family IsEmpty (a :: MyMaybe b) :: Bool
type instance IsEmpty (MyJust a) = False
type instance IsEmpty MyNothing = True
-}

-- Scoped type family 
type family Count (f :: *) :: Nat where
    Count (a -> b) = 1 + (Count b)
    Count x = 1

type family ToFunc (ts :: [*]) :: * where 
    ToFunc (t1:t2:'[]) = t1 -> t2
    ToFunc (t:ts) = t -> ToFunc ts
    

toFuncTst :: ToFunc [Int, Int, Int]
toFuncTst a b = a + b

