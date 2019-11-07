module Parallel.Repa where

import Data.Array.Repa
import Data.Kind (Type)


tst0 = fromListUnboxed (Z :. 5) [1..5] :: Array U DIM1 Int

tst1 = tst0 ! ix1 3
