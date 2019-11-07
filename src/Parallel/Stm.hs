{-# LANGUAGE TypeApplications #-}
module Parallel.Stm where

import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace (trace)
import Control.Concurrent.STM.TVar (TVar(..))
import Control.Concurrent.STM.TMVar (TMVar(..))

foo :: a -> STM a
foo = undefined

tst1 :: STM Int
tst1 = do
  a <- newTVar 0
  writeTVar a 55
  r <- readTVar a
  pure r

runTst1 = atomically tst1

-- block
tst2 = atomically $ do
  a <- newEmptyTMVar
  b <- readTMVar a
  pure b


tst3 :: IO ()
tst3 = do
  v <- newTVarIO 0
  t1 <- forkIO $ loop 100000 $ atomically (t1Worker v)
  t2 <- forkIO $ loop 1000000 $ atomically (t2Worker v)
  forkIO $ loopQ [t1, t2]
  pure ()
  where
    t1Worker :: TVar Int -> STM Int
    t1Worker tvr = do
      v <- readTVar tvr
      writeTVar tvr (v + 1)
      pure v
    t2Worker :: TVar Int -> STM String
    t2Worker tvr = do
      v <- readTVar tvr
      pure $ "snd " ++ show v
    loop :: Show a => Int -> IO a -> IO ()
    loop n ioa = do
      a <- ioa
      print a
      threadDelay n
      loop n ioa
    loopQ l = do
      c <- getChar
      if c == 'q'
        then mapM_ killThread l
        else loopQ l

-- orElse will call second arg if first calls retry internally
tst4 :: IO ()
tst4 = do
  v <- newEmptyTMVarIO @Int
  r <- atomically $ (fst v) `orElse` (snd v)
  print r
  where
    fst v = do
      i <- readTMVar v
      putTMVar v (i + 5)
      pure i
    snd v = do
      putTMVar v 999
      pure 999

data Async a = Async ThreadId (TMVar (Either String a))

async :: IO a -> IO (Async a)
async ioa = do
  var <- newEmptyTMVarIO
  thId <- forkIO $ do
    r <- ioa
    atomically $ putTMVar var (Right r)
  pure (Async thId var)

waitCatchSTM :: Async a -> STM (Either String a)
waitCatchSTM (Async tid tmv) = readTMVar tmv

waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM a
  case r of 
    Right a -> return a
    Left  e -> undefined -- throwSTM $ userException e
    
waitAny :: [Async a] -> IO a
waitAny l = atomically $ foldr orElse retry (fmap waitSTM l)


tst5 = let 
  ticker id 0 = print $ "finished " ++ show id
  ticker id n = do 
    threadDelay 1000000
    ticker id (n - 1)
  items = [1,2,4,5,6,3,2,6,5,1]
  in do
    threads <- mapM (\i -> async $ ticker i i) items
    waitAny threads