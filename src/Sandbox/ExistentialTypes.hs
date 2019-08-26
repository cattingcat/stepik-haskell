{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Sandbox.ExistentialTypes where

data Worker x y = forall b. Show b => Worker {
    buffer :: b, 
    input :: x, 
    output :: y
}

foo :: Worker String String -> String
foo w@(Worker b x y) = (show $ b) ++ (input w) ++ (output w)

-- foo $ Worker 1 "1" "2"


-- Rank2Types
f :: (forall a. [a] -> a) -> (Int, Char)
f get = (get [1,2], get ['a', 'b', 'c'])


-- ImpredicativeTypes
g :: Maybe (forall a. [a] -> a) -> (Int, Char)
g Nothing = (0, '0')
g (Just get) = (get [1,2], get ['a','b','c'])