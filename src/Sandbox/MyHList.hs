{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}


module Sandbox.MyHList where

import GHC.Exts (Constraint)
import GHC.TypeNats
import Data.Kind (Type)
import GHC.Natural (Natural)
import Data.Data (Proxy)
import Data.Type.Equality ((:~:) (..))


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
    
    
type family FixList (n :: Nat) (a :: Type) :: [Type] where 
  FixList 1 a = '[a]
  FixList n a = a : FixList (n - 1) a
  
type family FixHList (n :: Nat) (a :: Type) :: Type where
  FixHList n a = HList (FixList n a)
  
tstFixHList :: FixHList 5 Int
tstFixHList = 1 ::: 2 ::: 3 ::: 4 ::: 5 ::: HNil




tmp2 :: HList [String, Int, Char, String]
tmp2 = "He" ::: 11 ::: '0' ::: " world" ::: HNil

type family GetAt (n :: Nat) (l :: [Type]) :: Type where
  GetAt 0 (h:t) = h
  GetAt n (h:t) = GetAt (n - 1) t

headHl :: HList ts -> Maybe (GetAt 0 ts)
headHl HNil = Nothing
headHl (h ::: t) = Just h
