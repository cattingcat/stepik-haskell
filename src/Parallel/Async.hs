{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module Parallel.Async where

import Control.Concurrent (ThreadId, MVar, newEmptyMVar, putMVar, forkIO, throwTo, threadDelay, forkFinally)
import Control.Exception (try, catch, Exception, mask)
import Data.Data (Typeable)
import Control.Exception.Base (throw, SomeException, ErrorCall(..), bracket, throwIO, AsyncException(..), uninterruptibleMask)
import GHC.Exception.Type (divZeroException)
import Control.Concurrent.MVar (readMVar)
import System.IO (hSetBuffering, stdin, BufferMode(..))
import Control.Monad (forever, when)


data Async a = Async ThreadId (MVar (Either SomeException a))

async' :: IO a -> IO (Async a)
async' a = do
  mvar <- newEmptyMVar
  threadId <- forkIO $ do
    r <- try a
    putMVar mvar r
  pure $ Async threadId mvar

async :: IO a -> IO (Async a)
async action = do
  m <- newEmptyMVar
  t <- forkFinally action (putMVar m)
  pure (Async t m)


waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async _ r) = readMVar r

wait :: Async a -> IO a
wait (Async _ mvr) = do
  r <- readMVar mvr
  case r of
    Left e -> throwIO e
    Right a -> pure a

waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a1@(Async t1 r1) a2@(Async t2 r2) = do
  res <- newEmptyMVar
  forkIO $ do { r <- try (fmap Left (wait a1)); putMVar res r }
  forkIO $ do { r <- try (fmap Right (wait a2)); putMVar res r }
  wait (Async t1 res) -- todo: t1 isnt correct
  
withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io f = bracket (async io) cancel f 


--data ThreadKilled = ThreadKilled deriving (Show)
--instance Exception ThreadKilled

cancel :: Async a -> IO ()
cancel (Async tid _) = throwTo tid ThreadKilled

-- critical section
-- mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
--    mask $ \restore -> do
--        x <- acquire
--        restore (do_something_with x) `onException` release
--        release


tst1 = throw $ ErrorCall "kek" -- sake as error "kek"

tst2 = bracket (do print "start"; pure 6) (\m -> print ("end " ++ show m)) (\n -> print ("during " ++ show n))

tst3 = bracket (do print "start"; pure 6) (\m -> print ("end " ++ show m)) (\n -> throwIO $ ErrorCall ("err " ++ show n))


cancelListener :: IO () -> IO ThreadId
cancelListener quit = forkIO $ do
  hSetBuffering stdin NoBuffering
  forever $ do
    c <- getChar
    when (c == 'q') $ do { quit; throwIO ThreadKilled }

tst4 = do
  a1 <- async $ do { threadDelay 1000000; print "kek" }
  a2 <- async $ do { threadDelay 4000000; print "puk" } `catch` \(e :: SomeException) -> print ("err " ++ show e)
  cancelListener $ do { print "quit"; cancel a1; cancel a2 }

tst5 = do
  a1 <- async $ do { threadDelay 1000000; print "kek" }
  a2 <- async $ uninterruptibleMask (\restore -> do { threadDelay 4000000; print "puk"; restore $ pure () }) `catch` \(e :: SomeException) -> print ("err " ++ show e)
  cancelListener $ do { print "quit"; cancel a1; cancel a2 }



timeout :: Int -> IO a -> IO (Maybe a)
timeout delay m 
  | delay == 0 = pure Nothing
  | delay < 0 = Just <$> m
  | otherwise = do
  asyncM <- async m
  let cancelFunc = threadDelay delay >> cancel asyncM
  bracket (forkIO cancelFunc) (\cancelTId -> throwTo cancelTId ThreadKilled) (\_ -> Just <$> wait asyncM) `catch`
    (\(e :: SomeException) -> pure Nothing)

tst6 = timeout 10000000 (do { threadDelay 4000000; print "puk" })