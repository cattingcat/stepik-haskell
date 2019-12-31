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
{-# LANGUAGE FlexibleContexts #-}


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

type family AllM (m :: Type -> Type) (list :: [Type]) :: [Type] where
  AllM _ '[] = '[]
  AllM m (t:ts) = m t ': AllM m ts

--type family AllIO (list :: [Type]) :: [Type] where
--  AllIO '[] = '[]
--  AllIO (t:ts) = IO t ': AllIO ts

type AllIO (list :: [Type]) = AllM IO list

type family UnIO (list :: [Type]) :: [Type] where
  UnIO '[] = '[]
  UnIO (IO t : ts) = t : UnIO ts


class HTraversable (m :: Type -> Type) (a :: [Type]) where
  type Ret a :: [Type]
  hSequence :: HList a -> m (HList (Ret a))

instance Monad m => HTraversable m '[] where
  type Ret '[] = '[]
  hSequence HNil = pure HNil

instance (Monad m, HTraversable m ts) => HTraversable m (m t : ts) where
  type Ret (m t : ts) = t ': Ret ts
  hSequence (iot ::: iots) = do
    t <- iot
    ts <- hSequence iots
    pure (t ::: ts)

hSequence' :: HTraversable m ts => HList ts -> m (HList (Ret ts))
hSequence' = hSequence

-- requires FlexibleContexts because of explicit IO type
waitAll :: HTraversable IO ts => HList ts -> IO (HList (Ret ts))
waitAll = hSequence

tst :: HList (AllIO '[Int, String])
tst = pure 5 ::: pure "kek" ::: HNil

tst2 :: IO ()
tst2 = do
 t <- hSequence tst
 let i ::: s ::: HNil = t
 print $ i + 1
 print $ s <> "puk"

tst3 :: IO ()
tst3 = do
  t <- waitAll tst
  let i ::: s ::: HNil = t
  print $ i + 1
  print $ s <> "puk"