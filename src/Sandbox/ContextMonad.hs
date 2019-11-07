{-# LANGUAGE GADTs #-}

module Sandbox.ContextMonad where

newtype MyVar a = MyVar a deriving (Eq)

data StateInfo a = Var (MyVar a) | Pure a

data MyStateMon a where
 MsmVar :: a -> [StateInfo a] -> MyStateMon a
 MsmVVar :: MyVar a -> [StateInfo a] -> MyStateMon (MyVar a)
-- MsmF :: MyStateMon b -> (b -> a) -> MyStateMon a


newVar :: a -> MyStateMon (MyVar a)
newVar v = let vr = MyVar v in MsmVVar vr [Var vr]

--instance Functor MyStateMon where
--  fmap f (MsmVar v s) = MsmVar (f v) s

--instance Applicative MyStateMon where 
--  pure a = MsmVar a []
--  (<*>) (MsmVar f s1) (MsmVar v s2) = MsmVar (f v) (s1 ++ s2)
--  
--instance Monad MyStateMon where 
--  return = pure
--  (>>=) (MsmVar v s) f = let (MsmVar v' s') = f v in MsmVar v' (s ++ s')