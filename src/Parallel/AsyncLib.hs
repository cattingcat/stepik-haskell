module Parallel.AsyncLib where

import Control.Concurrent (forkFinally)

-- https://github.com/simonmar/async/blob/master/Control/Concurrent/Async.hs


tst = forkFinally (do print "kekpuk"; pure 5) (\a -> do print (show a); print "end")