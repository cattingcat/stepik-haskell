module Parallel.StmQueue where

import Control.Concurrent.STM (TVar, STM, newTVar, readTVar, writeTVar, retry, orElse)

data TBQueue a = TBQueue (TVar Int) (TVar [a]) (TVar [a])

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue n = do 
  cap   <- newTVar n
  read  <- newTVar []
  write <- newTVar []
  pure (TBQueue cap read write)
  
writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue c r w) a = do 
  cap <- readTVar c
  if cap == 0 then retry else do
    writeTVar c (cap - 1)
    write <- readTVar w
    writeTVar w (a:write)
    
readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue c r w) = do
  cap <- readTVar c
  readL <- readTVar r
  case readL of 
    (x:xs) -> do { writeTVar c (cap + 1); writeTVar r xs; pure x }
    _ -> do
      writeL <- readTVar w
      let reversed = reverse writeL
      case reversed of 
        [] -> retry
        (y:ys) -> do { writeTVar c (cap + 1); writeTVar r ys; pure y  }
  