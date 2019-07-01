module HsFp2.ZipListTask where
import Control.Applicative (ZipList(ZipList), getZipList)

-- Реализуйте операторы (>*<) и (>$<), позволяющие спрятать упаковку ZipList и распаковку getZipList:
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<$>) :: (a -> b) -> f a -> f b


(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f fa = getZipList $ f <$> (ZipList fa)

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) ff fa = getZipList $ (ZipList ff) <*> (ZipList fa)


test1 = (^2) >$< [1,2,3]
test2 = [(+1), (^2)] >*< [1,2,3,4]



divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0",1)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' xs