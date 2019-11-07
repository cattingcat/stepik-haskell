{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ThinkingWithTypes.FirstClassFamilies where

import Prelude hiding (fst)
import Data.Kind (Constraint, Type)

-- | Defunctionalization

fst :: (a, b) -> a
fst (a, b) = a

newtype Fst a b = Fst (a, b)

class Eval l t | l -> t where
  eval :: l -> t

instance Eval (Fst a b) a where
  eval (Fst (a, b)) = a

newtype ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe []) = Nothing
  eval (ListToMaybe (h:_)) = Just h

data MapList dfb a = MapList (a -> dfb) [a]

instance Eval dfb dft => Eval (MapList dfb a) [dft] where
  eval (MapList f []) = []
  eval (MapList f (h:t)) = eval (f h) : eval (MapList f t)
  
  
tst1 = eval ( MapList Fst [(" hello " , 1) , (" world " , 2) ])






-- | First class families

type Exp a = a -> Type
type family EvalFam (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b            -- :kind! Snd    = Snd :: (a, b) -> b -> *
type instance EvalFam (Snd '(a, b)) = b

--  :set -XDataKinds
--  :kind! EvalFam (Snd '(Int, "lel"))
--       valFam (Snd '(Int, "lel")) :: Symbol
--       = "lel"


data FromMaybe :: a -> Maybe a -> Exp a
type instance EvalFam (FromMaybe a   'Nothing) = a
type instance EvalFam (FromMaybe _1 ('Just a)) = a

data ListToMaybeK :: [a] -> Exp (Maybe a)
type instance EvalFam (ListToMaybeK '[]) = 'Nothing
type instance EvalFam (ListToMaybeK (a ': t)) = 'Just a

data MapListK :: (a -> Exp b) -> [a] -> Exp [b]
type instance EvalFam (MapListK f '[]) = '[]
type instance EvalFam (MapListK f (h ': t)) = EvalFam (f h) ': EvalFam (MapListK f t)

data FoldrK :: (a -> b -> b) -> b -> [a] -> Exp b
type instance EvalFam (FoldrK f z '[]) = z
type instance EvalFam (FoldrK f z (h ': t)) = f h (EvalFam (FoldrK f z t))

data Pure :: a -> Exp a
type instance EvalFam (Pure a) = a

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance EvalFam (f =<< a) = EvalFam (f (EvalFam a))
infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> (a -> Exp c)
type instance EvalFam ((f <=< g) a) = EvalFam (f (EvalFam (g a)))

type Snd2 = Snd <=< Snd
--  :kind! EvalFam (Snd2 '(1, '(2, 3)))          = 3
--  see package   first-class-families


data TyEq :: a -> b -> Exp Bool
type instance EvalFam (TyEq a b) = TypeEqImpl a b

type family TypeEqImpl (a :: k) (b :: k) where 
  TypeEqImpl a a = 'True
  TypeEqImpl a b = 'False


data Collapse :: [Constraint] -> Exp Constraint
type instance EvalFam (Collapse '[]) = (() :: Constraint)
type instance EvalFam (Collapse (a ': as)) = (a, EvalFam (Collapse as))

data Puref :: (a -> b) -> a -> Exp b
type instance EvalFam (Puref f a) = EvalFam (Pure (f a))

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapListK (Puref c) ts

-- :kind! EvalFam (All Eq '[Int, Bool])       =  (Eq Int, Eq Bool)






-- | Polymorphism

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance EvalFam (Map f '[]) = '[]
type instance EvalFam (Map f (a ': as)) = EvalFam (f a) ': EvalFam (Map f as)

type instance EvalFam (Map f ('Just a)) = 'Just (EvalFam (f a))
type instance EvalFam (Map f 'Nothing) = 'Nothing

type instance EvalFam (Map f ('Left x)) = 'Left x
type instance EvalFam (Map f ('Right a)) = 'Right (EvalFam(f a))

type instance EvalFam (Map f '(a, b)) = '(EvalFam (f a), b)



data Mappend :: a -> a -> Exp a

type instance EvalFam (Mappend '() '()) = '()
type instance EvalFam (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
--type instance EvalFam (Mappend (a :: [k]) (b :: [k])) = EvalFam (a ++ b)

data Mempty :: a -> Exp a

type instance EvalFam (Mempty '()) = '()
type instance EvalFam (Mempty (c :: Constraint)) = ()
type instance EvalFam (Mempty (l :: [k])) = '[]

