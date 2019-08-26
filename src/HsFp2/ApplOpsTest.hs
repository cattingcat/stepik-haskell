module HsFp2.ApplOpsTest where

import Control.Applicative ((<**>), ZipList(..), liftA2)

-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c

-- infixl 4 <**>
-- (<**>) :: Applicative f => f a -> f (a -> b) -> f b
-- (<**>) = liftA2 (flip ($))

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)


test11 = Just 1 <**> Just (+2)
test12 = Just 1 <*?> Just (+2)

-- Yep
test21 = [1,2] <**> [(\_ -> 1), (\_ -> 2)]
test22 = [1,2] <*?> [(\_ -> 1), (\_ -> 2)]

-- Yep
test31 = Left "AA" <**> Left "BB"
test32 = Left "AA" <*?> Left "BB"

test41 = ZipList [1,2] <**> ZipList [(+3),(+4)]
test42 = ZipList [1,2] <*?> ZipList [(+3),(+4)]

-- Yep
test51 = ("AA", 3) <**> ("B",(+1))
test52 = ("AA", 3) <*?> ("B",(+1))



tmp :: String -> (Int -> Int)
tmp s = (+5)

-- test61, test62 :: String -> Int
-- test61 = (length <**> tmp) "qwe"
-- test62 = (length <*?> tmp) "qwe"