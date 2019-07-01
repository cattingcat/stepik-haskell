{-# LANGUAGE InstanceSigs #-}  -- allows signatures in instance declaration
module HsFp2.MonadsAndEffects.ReaderTTasks.MonadTrans where

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where 
    fmap :: (a -> b) -> Reader e a -> Reader e b
    fmap f v = Reader $ \e -> f (runReader v e)

instance Applicative (Reader e) where 
    pure :: a -> Reader e a
    pure = Reader . const
    (<*>) af av = Reader $ \e -> let 
        f = runReader af e
        v = runReader av e
        in f v 

instance Monad (Reader e) where 
    return = pure
    (>>=) mv f = Reader $ \e -> let 
        v = runReader mv e 
        r2 = f v
        v2 = runReader r2 e
        in v2

ask :: Reader e e
ask = Reader id

asks :: (e -> a) -> Reader e a
asks = Reader



newtype ReaderT e m a = ReaderT { runReaderT :: e -> m a }

instance (Functor m) => Functor (ReaderT e m) where 
    fmap f v = ReaderT $ \e -> fmap f (runReaderT v e)

instance (Applicative m) => Applicative (ReaderT e m) where 
    pure = ReaderT . const . pure
    (<*>) aaf aav = ReaderT $ \e -> let 
        af = runReaderT aaf e
        av = runReaderT aav e in af <*> av
    -- (ReaderT aaf) <*> (ReaderT aav) = liftA2 (<*>) aaf aav

instance (Monad m) => Monad (ReaderT e m) where 
    return = pure
    (>>=) aam f = ReaderT $ \e -> (runReaderT aam e) >>= \a -> runReaderT (f a) e

asksT :: Monad m => (e -> a) -> ReaderT e m a
asksT f = ReaderT  (return . f)

askT :: Monad m => ReaderT e m e
askT = ReaderT return

liftT :: Monad m => m a -> ReaderT e m a
liftT ma = ReaderT $ \_ -> ma

type ReadMaybe e a = ReaderT e Maybe a

type ReadMaybeList = ReadMaybe [String] String

strList = ["qwe", "asd", "zxc", "rty", "fgh", "vbn"]

findFirst :: ReadMaybeList
findFirst = ReaderT $ \e -> if null e then Nothing else Just $ head e

findFirst' :: ReadMaybeList
findFirst' = do
    e <- askT
    if null e then liftT Nothing else asksT head

tst = runReaderT findFirst' strList