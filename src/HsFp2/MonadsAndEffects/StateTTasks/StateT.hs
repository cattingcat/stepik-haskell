{-# LANGUAGE InstanceSigs #-}  -- allows signatures in instance declaration
{-# LANGUAGE KindSignatures #-}

module HsFp2.MonadsAndEffects.StateTTasks.StateT where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Monoid
import Data.Char

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT $ return . f

get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> return ((), f s)


instance Functor m => Functor (StateT s m) where 
    fmap f st = StateT $ fmap upd . runStateT st where 
        upd ~(a, s) = (f a, s)

instance Monad m => Applicative (StateT s m) where 
    pure a = StateT $ \s -> return (a, s)
    stF <*> stV = StateT $ \s -> do
        ~(f, s')  <- runStateT stF s
        ~(v, s'') <- runStateT stV s'
        return (f v, s'')

instance Monad m => Monad (StateT s m) where 
    return = pure
    mv >>= f = StateT $ \s -> do
        ~(v, s') <- runStateT mv s
        let mb = f v
        runStateT mb s'
--    fail str = StateT $ \s -> fmap (\v -> (v, s)) (fail str)

-- instance MonadFail (StateT s m) where 
--     fail s = undefined

instance MonadTrans (StateT s) where 
    lift :: Monad m => m a -> StateT s m a 
    lift ma = StateT $ \s -> fmap (\a -> (a, s)) ma


evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT st = fmap fst . runStateT st

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT st = fmap snd . runStateT st

readerToStateT :: Monad m => ReaderT r m a -> StateT r m a
readerToStateT rd = StateT $ \s -> do 
    v <- runReaderT rd s
    return (v, s)

stTst1 :: StateT Int [] Int 
stTst1 = do
    v <- get
    put (v + 1)
    f <- StateT $ \s -> [((+1), s), ((+2), s)]
    return (f v)
runStTst1 = runStateT stTst1 0