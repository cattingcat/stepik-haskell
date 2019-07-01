module HsFp2.OddCont where

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

-- Functor, Foldable, Traversable

instance Functor OddC where
    fmap f (Un a) = Un (f a)
    fmap f (Bi a b x) = Bi (f a) (f b) (fmap f x)

instance Foldable OddC where
    foldMap f (Un a) = f a
    foldMap f (Bi a b x) = f a <> f b <> foldMap f x

instance Traversable OddC where 
    traverse f (Un a) = Un <$> f a
    traverse f (Bi a b x) = Bi <$> f a <*> f b <*> traverse f x
    sequenceA (Un a) = Un <$> a
    sequenceA (Bi a b x) = Bi <$> a <*> b <*> sequenceA x

instance Applicative OddC where
    pure = Un
    (<*>) ff af = do
        f <- ff
        a <- af
        return (f a)
    -- (<*>) (Un f) (Un a) = Un (f a)
    -- (<*>) af@(Un f) (Bi a b oc) = Bi (f a) (f b) (af <*> oc)
    -- (<*>) (Bi af bf aof) uaf@(Un a) = Bi (af a) (bf a) (aof <*> uaf)
    -- (<*>) (Bi af bf ocf) (Bi a b oc) = let
    --     r1 = Bi (af a) (af b) 
    --     r1' = (af <$> oc)
    --     r2 = Bi (bf a) (bf b) (bf <$> oc)
    --     r3 = concat3OC (ocf <*> pure a) (ocf <*> pure b) (ocf <*> oc)
    --     in r1 $ concat3OC r1' r2 r3

instance Monad OddC where
    (>>=) (Un a) f = f a
    (>>=) (Bi a b oc) f = concat3OC (f a) (f b) (oc >>= f)



cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3
cntInf = Bi 'A' 'B' cntInf


test1 = (+1) <$> cnt5   -- Bi 4 5 (Bi 2 3 (Un 43))
--test2 = toList cnt5     -- [3,4,1,2,42]
test3 = sum cnt5        -- 52
test4 = traverse (\x->[x+2,x-2]) cnt1 -- [Un 44,Un 40]


concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a)        (Un b)      oc = Bi a b oc
concat3OC (Bi a b oc)   od          oe = Bi a b (concat3OC oc od oe)
concat3OC (Un a)        (Bi b c od) oe = Bi a b (concat3OC (Un c) od oe)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un oa)                        = oa
concatOC (Bi oa ob ooc)    = concat3OC oa ob (concatOC ooc)

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
tstR = concat3OC tst1 tst2 tst3 -- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))


tst20 = concatOC $ Un (Un 42)
tst21 = Bi 'a' 'b' (Un 'c')
tst22 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst23 = Bi 'i' 'j' (Un 'k')
tst24 = concatOC $ Bi tst21 tst22 (Un tst23) -- Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))



tst31 = (Bi (+1) (+2) (Un (+3))) <*> (Bi 1 2 (Un 3))
tst31' = [(+1), (+2), (+3)] <*> [1,2,3]



{-
Functor
Applicative -> Alternative
Monad -> MonadPlus
-}