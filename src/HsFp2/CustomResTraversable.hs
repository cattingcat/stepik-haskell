{-# LANGUAGE ExistentialQuantification #-}

module HsFp2.CustomResTraversable where

data Result a = Ok a | Error String deriving (Eq,Show)

instance Functor Result where
    fmap f (Ok a)    = Ok (f a)
    fmap _ (Error a) = Error a

instance Applicative Result where
    pure = Ok
    (<*>) (Ok f)    (Ok a)    = Ok (f a)
    (<*>) (Error s) _         = Error s
    (<*>) _         (Error s) = Error s

instance Foldable Result where
    foldMap f (Ok a) = f a
    foldMap _ _      = mempty

    foldr f ini (Ok a) = f a ini
    foldr _ ini _        = ini


instance Traversable Result where
    traverse f (Ok a) = Ok <$> f a
    traverse f (Error s) = pure $ Error s 

    sequenceA (Ok fa) = Ok <$> fa
    sequenceA (Error s) = pure $ Error s



test1 = traverse (\x -> [x + 2, x - 2]) (Ok 5)         -- [Ok 7,Ok 3]
test2 = traverse (\x -> [x + 2, x - 2]) (Error "!!!")  -- [Error "!!!"]


data MyTuple a = forall b. MyTuple (b, a)

data Mtuple a b = Mtuple a b

instance Functor (Mtuple a) where
    fmap f (Mtuple k a) = Mtuple k (f a)

instance Monoid k => Applicative (Mtuple k) where
    pure a = Mtuple mempty a
    (<*>) (Mtuple k' f) (Mtuple k'' v) = Mtuple (k' <> k'') (f v)