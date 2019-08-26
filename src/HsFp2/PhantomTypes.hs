{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HsFp2.PhantomTypes where


newtype Temperature a = Temp Double deriving (Show, Eq, Num)

data Celsium
data Farenheit
data Kelvin

comfort :: Temperature Celsium
comfort = Temp 23.0

t2 = Temp 55.0

c2f :: Temperature Celsium -> Temperature Farenheit
c2f (Temp c) = Temp $ 1.8 * c + 32

k2c :: Temperature Kelvin -> Temperature Celsium
k2c (Temp k) = Temp $ k - 273.15