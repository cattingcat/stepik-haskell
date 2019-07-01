module HsFp2.TraversableTest where

data Triple a = Tr a a a  deriving (Eq, Show)

instance Functor Triple where
    fmap f (Tr a b c) = Tr (f a) (f b) (f c)

instance Applicative Triple where
    pure a = Tr a a a
    (<*>) (Tr fa fb fc) (Tr a b c) = Tr (fa a) (fb b) (fc c) 

instance Foldable Triple where
    foldMap f (Tr a b c) = f a <> f b <> f c

instance Traversable Triple where
    traverse f (Tr a b c) = Tr <$> f a <*> f b <*> f c
    sequenceA (Tr fa fb fc) = Tr <$> fa <*> fb <*> fc


test11 = foldl (++) "!!" (Tr "ab" "cd" "efg")  -- "!!abcdefg"
test12 = traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)  -- Right (Tr 12 14 16)
test13 = traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)  -- Left 8
test14 = sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))  -- Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)