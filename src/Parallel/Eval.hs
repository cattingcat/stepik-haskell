module Parallel.Eval where

import Control.Parallel.Strategies
import Control.DeepSeq

f :: [Int] -> Int
f [] = 0
f (x:xs) = fib x + f xs

fib 1 = 1
fib n = n + fib (n - 1)

-- returns immediately
tst1 = runEval $ do
  a <- rpar (f [1 .. 2000])
  b <- rpar (f [1..10])
  pure (a, b)
  
-- returns after b calculated to whnf
tst2 = runEval $ do
   a <- rpar (f [1 .. 2000])
   b <- rseq (f [1..10])
   pure (a, b)
   
-- returns when both in whnf
tst3 = runEval $ do
   a <- rpar (f [1 .. 2000])
   b <- rseq (f [1..10])
   rseq a
   pure (a, b)
   
tst4 = runEval $ do
   a <- rpar (f [1 .. 2000])
   b <- rpar (f [1..10])
   rseq b
   rseq a
   pure (a, b)
   
-- see also seq - forced calculation to whnf

--main = do
--  print tst1
--  print tst2
--  print tst3


parmap :: (a -> b) -> [a] -> Eval [b]
parmap f [] = pure []
parmap f (x:xs) = do 
  r <- rpar (f x)
  rs <- parmap f xs
  pure (r:rs)
  
tst5 :: [Int]
tst5 = runEval $ parmap fib [1..15000]

tst6 :: [Int]
tst6 = fmap fib [1..15000]

-- ./eval +RTS -N2 -s
-- stack ghc -- -O2 Eval.hs -o intro -rtsopts -threaded
-- see SPARKs section
--main = print (rnf tst6)

-- rnf - calculate to normal form


myPairPar :: (a, b) -> Eval (a, b)
myPairPar (a, b) = (,) <$> rpar a <*> rpar b

tst7 = (fib 55, fib 77) `using` myPairPar


tst8 :: [Int]
tst8 = withStrategy (parBuffer 50 rdeepseq) (fmap fib [1..1000])

main = print tst8