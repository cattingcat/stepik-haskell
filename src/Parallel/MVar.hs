module Parallel.MVar where

import Control.Concurrent
import Control.Monad
import System.IO
import Control.Exception.Base (ErrorCall(..))


-- | MVar can be written multiple times


-- take removes value from mvar
tst1 = do
  a <- newMVar 5
  b <- newEmptyMVar
  forkIO $ do { putMVar a 666; putMVar b 777 }
  av <- takeMVar a
  bv <- takeMVar b
  av2 <- takeMVar a
  print (av, bv, av2)
  
  
tst2 = do
  a <- newEmptyMVar --newMVar 5
  b <- newEmptyMVar
  forkIO $ do { putMVar a 666; putMVar b 777 } -- deadlock because of put to b (waits until read)
  av <- readMVar a
  bv <- takeMVar b
  print (av, bv)
  
  
tst3 = do 
  id <- forkIO $ do { threadDelay 1000000; print "HelloWorld" } 
  throwTo id (ErrorCallWithLocation "kek" "puk")
  print "canselled"