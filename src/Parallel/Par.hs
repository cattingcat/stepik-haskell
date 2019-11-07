module Parallel.Par where

import Control.Monad.Par
import Debug.Trace
import Control.DeepSeq

fib :: Int -> Int
fib 1 = 1
fib n = n + fib (n - 1)


-- | IVar can be written only once

tst1 = runPar $ do
  a <- new
  b <- new
  fork (put a (fib 10000))
  fork (put b (fib 10001))
  r1 <- get a
  r2 <- get b
  pure (r1, r2)

parFor' :: NFData b => (a -> Par b) -> [a] -> Par [b]
parFor' _ [] = pure []
parFor' f xl = let
  tmp x t = fork $ do {r <- (f x); put t r}
  ivl = sequence $ fmap (\i -> do {t <- new; tmp i t; pure t}) xl
 in do
  iv <- ivl
  mapM get iv


parMap' :: NFData b => (a -> b) -> [a] -> [b]
parMap' f arr = let
--  pf :: a -> Par (IVar b)
  pf i = do {t <- new; fork (put t (f i)); pure t}

--  pl :: Par [IVar b]
  pl = mapM pf arr

  in runPar $ do
    l <- pl
    mapM get l



arr :: [Int]
arr = [500000..500005]

tst2 = runPar $ parFor' (\i -> trace (show i) (pure $ fib i)) arr

tst3 = runPar $ parMapM (\i -> trace (show i) (pure $ fib i)) arr

tst4 = parMap' (\i -> trace (show i) (fib i)) arr

tst5 = fmap (\i -> trace (show i) (fib i)) arr


data IList a = Nil | Cons a (IVar (IList a))
type ParStream a = IVar (IList a)

instance NFData a => NFData (IList a) where
  rnf Nil = ()
  rnf (Cons a t) = rnf a

fromList :: NFData a => [a] -> Par (ParStream a)
fromList xs = do
  v <- new
  fork $ loop v xs
  pure v
  where
    loop iv [] = put iv Nil
    loop iv (x:xs) = do
      tailv <- new
      put iv (Cons x tailv)
      loop tailv xs

takeN :: Int -> Par (ParStream a) -> Par [a]
takeN n s = do
  str <- s
  foo n str

foo :: Int -> IVar (IList a) -> Par [a]
foo 0 _ = pure []
foo n iv = do
  l <- get iv
  case l of
    Nil -> pure []
    Cons a t -> do
      tl <- foo (n - 1) t
      pure (a:tl)


mapParS :: NFData b => (a -> b) -> ParStream a -> ParStream b
mapParS f str = runPar $ do 
  os <- new
  fork $ loop os str
  pure os  
  where 
    loop o s = do 
      sr <- get s
      case sr of 
        Nil -> put o Nil
        Cons h t -> do
          ot <- new
          put o $ Cons (f h) ot
          loop ot t

tst6 = runPar $ takeN 5 $ fromList $ fmap fib [11111..22222]