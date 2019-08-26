{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}


module Sandbox.SimpleDataPromotion where
import GHC.TypeLits
import Data.Type.Equality

data ListMark = Empty | NotEmpty 

data MList m a where
    MNil :: MList Empty a
    MCons :: a -> MList m a -> MList NotEmpty a

mhead :: MList NotEmpty a -> a
mhead (MCons a _) = a


tst1 = mhead (MCons 1 MNil)
-- tst11 = mhead MNil -- type mismatch


-- Constraints over promoted types
data Foo :: Nat -> * where
  Small    :: (n <= 2)  => Foo n
  Big      :: (3 <= n) => Foo n

  EmptyFoo    :: ((n == 0) ~ True) => Foo n
  NonEmptyFoo :: ((n == 0) ~ False) => Foo n


-- Similar constraint for MList
data MListBar a where
    Bar :: (m ~ NotEmpty) => MList m a -> MListBar a

tst2 = Bar (MCons 1 MNil)
-- tst21 = Bar MNil -- wont compile because of constraint

-- TupleS :: * -> * -> *
data TupleS a b where 
    TupleS ::  a -> b -> TupleS a b

data TupleD :: (*, *) -> * where 
    TupleD ::  a -> b -> TupleD '(a, b)