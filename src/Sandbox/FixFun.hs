module Sandbox.FixFun where
    
import Control.Monad.Fix

testList :: [Int]
testList = [1..99]

-- fix :: (a -> a) -> a
-- fin f = f(f(f(f(f(f(f(...)))))))

-- fix :: ((a -> b) -> a -> b) -> a -> b

sumList :: [Int] -> Int
sumList is = fix foo is where
    foo f (a : []) = a
    foo f (a : as) = a + f as

type Hungry a = Int -> a

hungry :: Hungry a
hungry = fix foo where 
    foo f n = f