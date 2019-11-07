{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Parallel.HigherAbsConcurrency where

import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread, throwTo)
import Control.Exception.Base (bracket, try, SomeException, Exception)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM
import Debug.Trace (trace)
import Control.Concurrent.STM.TVar (TVar(..))
import Control.Concurrent.STM.TMVar (TMVar(..))

timeout :: Int -> IO a -> IO (Maybe a)
timeout n io 
  | n < 0 = fmap Just io
  | n == 0 = pure Nothing
  | otherwise = do
    r <- race (threadDelay n) io
    case r of 
      Left _ -> pure Nothing
      Right a -> pure $ Just a
      
      
newtype InterruptException = InterruptException String deriving (Show)
instance Exception InterruptException


data Async a = Async ThreadId (TMVar (Either SomeException a))

async :: IO a -> IO (Async a)
async ioa = do
  var <- newEmptyTMVarIO
  thId <- forkIO $ do
    r <- ioa
    atomically $ putTMVar var (Right r)
  pure (Async thId var)

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async tid tmv) = readTMVar tmv

waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of 
    Right a -> return a
    Left  e -> throwSTM e
    
waitAny :: [Async a] -> IO a
waitAny l = atomically $ foldr orElse retry (fmap waitSTM l)

  
cancel :: Async a -> IO ()
cancel (Async tid _) = throwTo tid (InterruptException "")

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $ (fmap Left $ waitSTM a) `orElse` (fmap Right $ waitSTM b)

waitBoth :: Async a -> Async b -> IO (a, b)
waitBoth a b = atomically $ do 
  ra <- waitSTM a `orElse` (do { waitSTM b; retry })
  rb <- waitSTM b
  pure (ra, rb)

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io f = bracket (async io) cancel f

race :: IO a -> IO b -> IO (Either a b)
race ioa iob = withAsync ioa (\a -> withAsync iob (\b -> waitEither a b))


tst1 = timeout 50000 (do threadDelay 200; print "kek")

tst2 :: IO (Either Int Int)
tst2 = do
 a1 <- (async $ do threadDelay 200; print "kek"; pure 1) 
 a2 <- (async $ do threadDelay 300; print "puk"; pure 2)
 waitEither a1 a2