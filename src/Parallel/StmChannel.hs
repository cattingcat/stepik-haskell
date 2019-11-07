{-# LANGUAGE CPP #-}

module Parallel.StmChannel where

import Control.Concurrent.STM (TVar, STM, newTVar, readTVar, writeTVar, retry, orElse, atomically)
import Control.Concurrent(forkIO, threadDelay, killThread)

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

-- unpack - removes packing an uses raw C-structures
-- !  - removes laziness
#define unp(x) {-# UNPACK #-} !(x)

data TChan a = TChan unp(TVar (TVarList a)) unp(TVar (TVarList a))

newTChan :: STM (TChan a)
newTChan = do
  hole  <- newTVar TNil
  read  <- newTVar hole
  write <- newTVar hole
  pure (TChan read write)


readTChan :: TChan a -> STM a
readTChan (TChan r w) = do
  listHead <- readTVar r
  head <- readTVar listHead
  case head of
    TNil -> retry
    TCons a tail -> do
      writeTVar r tail
      pure a

lookTChan :: TChan a -> STM (Maybe a)
lookTChan (TChan r w) = do
  listHead <- readTVar r
  head <- readTVar listHead
  case head of
    TNil -> pure Nothing
    TCons a _ -> pure $ Just a

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan r w) a = do
  newEnd <- newTVar TNil
  end <- readTVar w
  writeTVar w newEnd
  writeTVar end (TCons a newEnd)

unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan r w) a = do
  listHead <- readTVar r
  newHead <- newTVar (TCons a listHead)
  writeTVar r newHead

readEitherTChan :: TChan a -> TChan b -> STM (Either a b)
readEitherTChan ca cb = (fmap Left (readTChan ca)) `orElse` (fmap Right (readTChan cb))






tst :: IO ()
tst = do
  chan <- atomically newTChan
  t1 <- forkIO $ loop 1000000 $ atomically (t1Worker chan)
  t2 <- forkIO $ loop 500000 $ atomically (t2Worker chan)
  forkIO $ loopQ [t1, t2]
  pure ()
  where
    t1Worker :: TChan Int -> STM String
    t1Worker tvr = do
      v <- readTChan tvr
      pure ("reader " ++ show v)
    t2Worker :: TChan Int -> STM String
    t2Worker tvr = do
      v <- lookTChan tvr
      case v of
        Nothing -> do { writeTChan tvr 0; pure "wrtr (empty)" }
        Just a -> do { writeTChan tvr (a + 1); pure $ "wrtr " ++ show a }
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