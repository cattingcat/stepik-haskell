{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module ExtTest.StTest where

import Control.Monad.ST
import Data.STRef
import Debug.Trace (trace)



foo :: (Ord b) => forall s . STRef s b -> (a -> b) -> a -> ST s b
foo state f a = do
  stateVal <- readSTRef state
  let val = f a
  if val > stateVal
  then writeSTRef state val
  else pure ()
  readSTRef state


testFoo :: Int
testFoo = runST $ do
  state <- newSTRef 0
  res1 <- foo state (+1) 5
  trace (show res1) (pure ())
  res2 <- foo state (+1) 4
  trace (show res2) (pure ())
  res3 <- foo state (+1) 6
  trace (show res3) (pure ())
  pure res3



data Dyn = forall a . Show a => Dyn a

deriving instance Show Dyn

bar :: Dyn
bar = Dyn "qweeqw"





--foo fn x = runST $ do
--  state <- newSTRef mempty
--
--  stateVal <- readSTRef state
--
--  let val = fn x
--  if stateVal /= val
--  then do
--    writeSTRef state val
--    readSTRef state
--  else readSTRef state






--  pure undefined
