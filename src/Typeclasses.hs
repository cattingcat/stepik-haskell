module Typeclasses where
import Prelude(($))


class Monoid m where
    mempty :: m
    mappend :: m -> m -> m

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)

class Functor f => Pointed f where
    pure :: a -> f a

class Pointed f => Applicative f where
    (<*>) :: f (a -> b) -> f a -> f b

    fmap2 :: (a -> b -> c) -> (f a -> f b -> f c)
    fmap2 f fa fb = (fmap f fa) <*> fb

    fmap3 :: (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
    fmap3 f fa fb fc = fmap2 f fa fb <*> fc

class Applicative f => Alternative f where 
    empty :: f a
    (<|>) :: f a -> f a -> f a

class Pointed m => Monad m where
    join :: m (m a) -> m a

    (>>=) :: m a -> (a -> m b) -> m b
    (>>=) ma f = join $ fmap f ma

class (Alternative m, Monad m) => MonadPlus m where
    mzero :: m a
    mzero = empty

    mplus :: m a -> m a -> m a
    mplus = (<|>)
