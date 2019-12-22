{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sandbox.Exs where

import Control.Monad.Trans.Writer

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe (Nothing:xs) = Nothing
flipMaybe (Just a:xs) =
  case flipMaybe xs of
    Just ys -> Just $ a:ys
    Nothing -> Nothing


tst1 = flipMaybe [Just 1, Nothing, Just 2]
tst2 = flipMaybe [Just 1, Just 3, Just 2]

--sum :: Int -> Int -> Int
--sum a b = a + b

sum :: Monad m => Int -> Int -> WriterT String m Int 
sum a b = do 
  let res = a + b
  tell $ " a + b = " <> show res
  pure res
  
  
  
data NonEmptyList a = a :| [a]

listToNonEmpty :: [a] -> Maybe (NonEmptyList a)
listToNonEmpty [] = Nothing
listToNonEmpty (a:as) = Just $ a :| as
 
  
data DistanceUnits = Meter | Inch deriving (Show, Eq)
newtype Distance (measure :: DistanceUnits) = Distance Double 
  deriving (Eq, Num, Fractional, Show)

distanceToMars :: Distance Meter 
distanceToMars = 534532

distanceToSun :: Distance Inch 
distanceToSun = 4564345534532
  
  
data Commission
data CombineStrategy

data TaskId
data Importance
  
f :: Commission -> Commission -> CombineStrategy -> Commission
g :: TaskId -> Importance -> IO ()



f = undefined
g = undefined




data Complex = Complex {
  re :: Double,
  im :: Double,
  
  d :: Maybe Int
}

--tst3 :: Maybe TaskId -> Maybe TaskId -> Bool
--tst3 a b = let 
--  c = a `compare` b
--  in False 


--data Tree a = Nil | Branch (Tree a) a (Tree a) 
--
--instance Eq a => Eq (Tree a) where 
--  (==) Nil Nil = True
--  (==) (Branch l1 a r1) (Branch l2 b r2) = 
--    a == b && l1 == l2 && r1 == r2

data List a = Nil | Cons a (List a)



--max :: Int -> Int -> Int
--max m n = if m > n then m else n

max :: Ord a => a -> a -> a
max a b = if a > b then a else b

--max (Just 2) (Just 3)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False


head :: [a] -> Maybe a
head [] = Nothing
head (a:_) = Just a


firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just a : _) = Just a
firstJust (Nothing : as) = firstJust as


newtype MyType = MyType Int 
  deriving (Eq, Ord, Num)


