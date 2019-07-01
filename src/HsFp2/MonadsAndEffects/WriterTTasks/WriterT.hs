{-# LANGUAGE InstanceSigs #-}  -- allows signatures in instance declaration
{-# LANGUAGE KindSignatures #-}

module HsFp2.MonadsAndEffects.WriterTTasks.WriterT where
import Control.Monad.Trans
import Data.Monoid
import Data.Char

-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- writer :: (a, w) -> Writer w a
-- writer = Writer


newtype WriterT w (m :: * -> *) a = WriterT { runWriterT :: m (a, w) }

writer :: Monad m => (a, w) -> WriterT w m a
writer = WriterT . return

execWriterT :: Monad m => WriterT w m a -> m w
execWriterT = fmap snd . runWriterT

tell :: Monad m => w -> WriterT w m ()
tell w = writer ((), w)

listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
listen w = WriterT $ do 
    ~(a, w) <- runWriterT w
    return ((a, w), w)

censor :: Monad m => (w -> w) -> WriterT w m a -> WriterT w m a
censor f w = WriterT $ do
    ~(a, w) <- runWriterT w
    return (a, f w)

instance Functor m => Functor (WriterT w m) where
    fmap :: (a -> b) -> WriterT w m a -> WriterT w m b
    fmap f (WriterT ma) = WriterT $ fmap upd ma where 
        upd ~(a, w) = (f a, w)


instance (Monoid w, Applicative m) => Applicative (WriterT w m) where 
    pure :: a -> WriterT w m a
    pure a = WriterT . pure $ (a, mempty)

    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    (WriterT f) <*> (WriterT v) = WriterT $ ap <$> f <*> v where
        ap :: Monoid w => (a -> b, w) -> (a, w) -> (b, w)
        ap ~(f, fw) ~(a, aw) = (f a, fw <> aw)


instance (Monoid w, Monad m) => Monad (WriterT w m) where 
    return = pure

    (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    ma >>= f = WriterT $ do
        ~(a, w) <- runWriterT ma
        ~(b, w') <- runWriterT (f a)
        return (b, w <> w')

instance Monoid w => MonadTrans (WriterT w) where
    lift :: Monad m => m a -> WriterT w m a
    lift ma = WriterT $ do 
        a <- ma
        return (a, mempty)



tst1 = writer ("res", ["log"]) :: WriterT [String] Maybe String

tst2 :: WriterT [String] Maybe Int
tst2 = do
    a <- writer (5, ["kek"]) 
    b <- if a >= 5 then lift (Just 6) else writer (77, ["puk"])
    tell ["Ololo"]
    return (a + b)

tst3 = censor (fmap (fmap toLower)) tst2

runTst2 = runWriterT tst2
runTst3 = runWriterT tst3
