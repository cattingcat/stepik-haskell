{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}


module Sandbox.MyHList where

import GHC.Exts (Constraint)


infixr 5 :::

data HList (ts :: [*]) where 
    HNil :: HList '[]
    (:::) :: t -> HList ts -> HList (t ': ts)

tmp :: HList [String, Int, Char, String]
tmp = "He" ::: 11 ::: '0' ::: " world" ::: HNil

type family Map (f :: a -> b) (ts :: [a]) :: [b] where 
    Map _ '[] = '[]
    Map f (t:ts) = f t ': Map f ts

type family Constraints (cs :: [Constraint]) :: Constraint where 
    Constraints '[] = ()
    Constraints (c:cs) = (c, Constraints cs)

type AllHave (c :: k -> Constraint) (ts :: [k]) = Constraints (Map c ts)

showHList :: AllHave Show ts => HList ts -> [String]
showHList HNil = []
showHList (x ::: xs) = (show x) : showHList xs


instance (AllHave Show ts) => Show (HList ts) where 
    show = show . showHList