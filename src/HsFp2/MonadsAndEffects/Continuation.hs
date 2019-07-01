module HsFp2.MonadsAndEffects.Continuation where

decode :: (Int -> c) -> c
decode c = c 0

as :: Int -> (Int -> c) -> c
as n c = c n

a :: Int -> (Int -> c) -> c
a n c = c n

number = id

one :: Int -> (Int -> c) -> c
one n c = c $ 1 + n

two :: Int -> (Int -> c) -> c
two n c = c $ n + 2

three :: Int -> (Int -> c) -> c
three n c = c $ n + 3

seventeen :: Int -> (Int -> c) -> c
seventeen n c = c $ n + 17

twenty :: Int -> (Int -> c) -> c
twenty n c = c $ n + 20

hundred :: Int -> (Int -> c) -> c
hundred n c = c $ n * 100

thousand :: Int -> (Int -> c) -> c
thousand n c = c $ n * 1000


tst10 = decode one as a number
tst12 = decode one hundred
tst13 = decode three thousand
tst14 = decode three thousand seventeen id
tst15 = decode three thousand twenty id
tst16 = decode three thousand twenty one id

tst1 = decode one hundred twenty three   as a number -- 123
tst2 = decode one hundred twenty one     as a number -- 121
tst3 = decode one hundred twenty         as a number -- 120
tst4 = decode one hundred                as a number -- 100
tst5 = decode three hundred              as a number -- 300
tst6 = decode two thousand seventeen     as a number -- 2017

square :: Int -> (Int -> c) -> c
square n c = c $ n^2

plusOne :: Int -> (Int -> c) -> c
plusOne n c = c $ n + 1

plus :: Int -> Int -> (Int -> c) -> c
plus m n c = c $ m + n

mul :: Int -> Int -> (Int -> c) -> c
mul m n c = c $ m * n

idC :: Int -> (Int -> c) -> c
idC n c = c n

t = square 3 plusOne idC idC idC
