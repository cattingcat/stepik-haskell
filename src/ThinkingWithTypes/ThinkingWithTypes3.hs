{-# LANGUAGE RankNTypes #-}

module ThinkingWithTypes.ThinkingWithTypes3 where

import GHC.Base
import GHC.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO (unsafePerformIO)

newtype ST s a = ST { unsafeRunST :: a }

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure a = seq a . ST $ a
  liftA2 f (ST a) (ST b) = seq f . seq a . seq b . ST $ f a b

instance Monad (ST s) where
  return = pure
  ST a >>= f = seq a . seq f $ f a

newtype STRef s a = STRef { unSTRef :: IORef a }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  v <- readSTRef ref
  writeSTRef ref (f v)


runST :: (forall s . ST s a) -> a
runST = unsafeRunST


safeExample :: ST s String
safeExample = do
  ref <- newSTRef "hello"
  modifySTRef ref (++ " world")
  readSTRef ref

tst1 = runST safeExample

-- | you can use STRef's only inside os ST monad, because of s (ST-trick)
--  tst2 = runST ( newSTRef True )  -- won't compile
-- 
-- runST makes new scope for s type-variable
--  runST :: (forall s . ST s a) -> a
--  runST :: (forall s . ST s (STRef s a) -> STRef s a   <- s out of scope