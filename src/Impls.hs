module Impls where
import Data.Char
import Data.Tuple
import Data.List

main :: IO ()
main = putStr "Hello"


fib :: Int -> Int 
fib n = foo 0 1 n where 
    foo a _ 0 = a
    foo a b n | n > 0 = foo b (a+b) (n-1)
    foo a b n | n < 0 = foo (b-a) a (n+1)
    
seqA :: Int -> Int 
seqA n = foo 1 2 3 n where 
    foo a _ _ 0  = a
    foo a b c n' = let d = b + c - 2 * a in foo b c d (n' - 1)

snc :: Int -> (Int, Int)
snc n | n < 0 = snc (-n)
snc n | n > 0 = let 
    m = quot n 10 in if m == 0 then (n, 1) else let
        d = n - 10 * m
        (s, c) = snc m in (s + d, c + 1)


integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let 
    n = 1000
    h = (b - a) / n
    sides = ()
    others acc x 0 = acc
    others acc x n = others (f x + acc) (x + h) (n-1)
    in h * (f a + f b + 2 * others 0 (a+h) (n-1)) / 2


-- integration :: (Double -> Double) -> Double -> Double -> Double
-- integration f a b = let 
--     n = 1000
--     step = (b - a) / n
--     sides = (f a + f b) * step / 2
--     xs = generate a b step
--     ys = fmap f xs
--     others = foldl (+) 0 ys
--     in sides + others * step

-- generate :: Double -> Double -> Double -> [Double]
-- generate a b s | abs (a - b) < abs s = []
-- generate a b s = let e = a + s in e:(generate e b s) 


--xi = a + (b-a)/n = (an + bi - ai) / n


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 as bs cs = let 
    pick [] = (0, [])
    pick (a':as') = (a', as')
    (a, at) = pick as
    (b, bt) = pick bs
    (c, ct) = pick cs in a + b + c : sum3 at bt ct


takeWhileEq :: Eq a => [a] -> ([a], [a])
takeWhileEq [] = ([], [])
takeWhileEq as@(a:_) = loop a as []  where
    loop i l@(a:as) acc = if i == a then loop i as (i:acc) else (acc, l)
    loop i []       acc = (acc, [])

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems a = let (al, as) = takeWhileEq a in al : (groupElems as)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (a:[]) = [a]
qsort (a:b:[]) = if a < b then [a,b] else [b,a]
qsort as@(a:_) = let 
    ls = filter (<a) as
    ms = filter (==a) as
    rs = filter (>a) as in (qsort ls) ++ ms ++ (qsort rs)


merge :: Ord a => [a] -> [a] -> [a]
merge []       m        = m
merge l       []        = l
merge l@(a:as) m@(b:bs) = if a < b then a : merge as m else b : merge l bs

perms :: [a] -> [[a]]
perms [] = []
perms [a] = [[a]]
perms (a:as) = concatMap (inserts a) (perms as)

inserts :: a -> [a] -> [[a]]
inserts a [] = [[a]] 
inserts i (a:as) = (i:a:as) : (fmap (a:) (inserts i as))


perms2 :: [a] -> [[a]]
perms2 [] = [[]]
perms2 (x:xs) = let
    len = length xs
    xperm p n = let (l, r) = splitAt n p in l ++ [x] ++ r
    xperms p = map (xperm p) [0..len]
  in concatMap xperms $ perms2 xs

delAllUpper :: String -> String
delAllUpper = unwords . (filter (any isLower)) . words

fibs :: [Integer]
fibs = 0:1:zipWith (+) fibs (tail fibs)

data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
    succ (Odd a) = Odd $ a + 2
    pred (Odd a) = Odd $ a - 2
    enumFrom = iterate succ
    enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
    enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
    enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x , y .. z]

    toEnum x = Odd $ toInteger x * 2 + 1
    fromEnum (Odd x) = quot (fromInteger x - 1) 2

meanList :: [Double] -> Double
meanList = foo . foldr (\x (s, n) -> (s + x, n + 1)) (0,0) where 
    foo (s, n) = s / n


evenOnly' :: [a] -> [a]
evenOnly' l = fst $ foldr (\x (agg, i) -> if even i then (x:agg, i-1) else (agg, i-1) ) ([], length l) l


-- evenOnly :: [a] -> [a]
-- evenOnly a = foldr (\(x, i) acc -> if even i then x:acc else acc) [] (zip a [1 ..])


-- evenOnly :: [a] -> [a]
-- evenOnly = foldr 

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g (a,z) | a <= z = Just (z, (a, pred z))
                | otherwise = Nothing
                