{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Sandbox.DataFamily where

import Data.Kind (Type)


class Kek a (c :: Type -> Type) where
  data Puk a b c
  init :: b -> Puk a b c
  foo :: b -> Puk a b c -> a


bar :: Puk a b c -> Int
bar _ = 66


instance Kek Int [] where
  data Puk Int b [] = MyType [Int] b

  init b = MyType [1,2,3] b

  foo :: b -> Puk Int b [] -> Int
  foo i arr = 66

instance Kek Char [] where
  data Puk Char b [] = MyType2 [Char]

  init _ = MyType2 []

  foo :: b -> Puk Char b [] -> Char
  foo i arr = 'c'

tst0' = bar $ MyType [1,2] "wew"
tst1' = bar $ MyType2 "qwq"


-- specialization like
data family Df a b
data instance Df Int b = Df1 Int b