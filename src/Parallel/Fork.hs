module Parallel.Fork where

import Control.Concurrent
import Control.Monad
import System.IO

main = do
  hSetBuffering stdout NoBuffering
  forkIO (replicateM_ 100000 (putChar 'A'))
  replicateM_ 100000 (putChar 'B')

tst1 = threadDelay 2000000 >> putStrLn "Hi"