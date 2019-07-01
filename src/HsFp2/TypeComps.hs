{-# LANGUAGE TypeOperators #-}

module HsFp2.TypeComps where

infixr 9 |.|

newtype (|.|) f g a = Cmps { getCmps :: f (g a) } deriving (Show, Eq)
-- :k (|.|)   =    (|.|) :: (* -> *) -> (* -> *) -> * -> *

instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap ff (Cmps fga) = Cmps $ fmap (fmap ff) fga

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure = Cmps . pure . pure
    (<*>) (Cmps ff) (Cmps fa) = Cmps $ (<*>) <$> ff <*> fa
    -- (<*>) (Cmps ff) (Cmps fa) = Cmps $ foo <$> ff <*> fa where
    --     foo ff' fa' = ff' <*> fa' 


newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } deriving (Eq,Show) 

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
    fmap ff (Cmps3 fgha) = Cmps3 $ fmap (fmap (fmap ff)) fgha



type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

-- ((Integer, *) |.| (Char, *)) Bool = (Integer, (Char, Bool))
testA :: A 
testA = Cmps $ (1, ('1', True))

-- ((Bool, (t -> t), *) |.| Either String *) Int = (Bool, (t -> t), Either String Int)
testB :: B t 
testB = Cmps $ (True, id, Right 5)

-- (|.|) ((Bool -> *) ) ((Integer -> *) ) Integer = Bool -> Integer -> Integer
testC :: C
testC = Cmps $ \b i -> if b then i else 0

test1 :: (Maybe |.| []) Char
test1 = Cmps $ Just "Qwe"

unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 fgha = fmap getCmps (getCmps fgha)

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
-- unCmps4 f2f1gha = fmap (\x -> (fmap getCmps) (getCmps x)) (getCmps f2f1gha)
unCmps4 f2f1gha = fmap unCmps3 (getCmps f2f1gha)
