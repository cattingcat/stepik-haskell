{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sandbox.RepresentableTest where

import Debug.Trace
import Data.Char

data Stream x = Cons x (Stream x)
newtype Reader r a = R { unR :: r -> a }

class Representable f where
  type Rep f :: *
  tabulate :: (Rep f -> x) -> f x
  index :: f x -> Rep f -> x

instance Representable Stream where
  type Rep Stream = Integer
  tabulate f = Cons (f 0) (trace "calculation" $ tabulate (f . (+ 1)))
  index (Cons b bs) n = if n == 0 then b else index bs (n - 1)

memo :: Stream Integer
memo = tabulate (\x -> x*x)

memoizedSqr :: Integer -> Integer
memoizedSqr = index memo

instance Representable (Reader r) where
  type Rep (Reader r) = r
  tabulate f = R f
  index (R f) = f

main = print 10


tst :: String -> Bool
tst s = let (l, u) = foldr foo (0, 0) s in l == u where
  foo s (l, u)
    | isLower s = (l + 1, u)
    | isUpper s = (l, u + 1)
    | otherwise = (l, u)