{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Sandbox.DataKindsFun where

import GHC.Exts (Constraint)

tst :: String -> Int
tst = \case 
    "" -> 1
    "a" -> 2
    _ -> 3

data S a b = L a | R b

type Ll = 'L


-- foo :: Ll Int -> String
-- foo a = "qwe"

data SomeData (f :: k -> *) (a :: k) where 
    MkSomeData :: f a -> SomeData f a

type family TreeKey a :: Constraint
type instance TreeKey Int = ()
type instance TreeKey String = ()
type instance TreeKey (Maybe a) = (Ord a)

-- ConstraintKinds
treeKeyFoo :: TreeKey a => a -> Int
treeKeyFoo _ = 55

-- https://stackoverflow.com/questions/31317159/constraintkinds-explained-on-a-super-simple-example



data MyMaybe a = MyJust a | MyNothing

type family IsEmpty (a :: MyMaybe b) :: Bool
type instance IsEmpty (MyJust a) = False
type instance IsEmpty MyNothing = True

newtype ExType a b = ExType b

isEmptyTstFoo :: ExType True b -> b
isEmptyTstFoo (ExType b) = b

kek :: ExType (IsEmpty (MyJust Int)) Int 
kek = ExType 55


data Trio :: * -> * -> * -> * where 
    MkTrio :: a -> b -> c -> Trio a b c

newtype TrioNewType a b = TrioNewType b

trioTst :: TrioNewType (MkTrio Int String Char) Int -> String
trioTst = undefined