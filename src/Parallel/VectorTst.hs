{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Parallel.VectorTst where

import Control.Monad.ST
import Data.STRef
import Data.Vector as V
import Data.Vector.Mutable as MV

tst1 = V.create $ do
  v <- MV.replicate 5 666
  MV.write v 3 111
  pure v


tst2 = runST $ do
  n <- newSTRef 6
  pure 6