module Sandbox.ReaderWriter where
import Data.Monoid

-- ReaderT e (Writer w) a
newtype ReaderWriter e w a = ReaderWriter { runRW :: e -> (a, w) }

instance Functor (ReaderWriter e w) where 
    fmap f (ReaderWriter ma) = ReaderWriter $ \e -> let 
        ~(a, w) = ma e
        in (f a, w)

instance Monoid w => Applicative (ReaderWriter e w) where 
    pure a = ReaderWriter $ \_ -> (a, mempty)
    (ReaderWriter mf) <*> (ReaderWriter ma) = ReaderWriter $ \e -> let
        ~(f, w) = mf e
        ~(a, w') = ma e
        in (f a, w <> w')

instance Monoid w => Monad (ReaderWriter e w) where 
    return = pure
    (ReaderWriter ma) >>= f = ReaderWriter $ \e -> let
        ~(a, w) = ma e
        mb = f a
        ~(b, w') = runRW mb e
        in (b, w <> w')

ask :: Monoid w => ReaderWriter e w e
ask = ReaderWriter $ \e -> (e, mempty)

asks :: Monoid w => (e -> a) -> ReaderWriter e w a
asks f = ReaderWriter $ \e -> (f e, mempty)

tell :: w -> ReaderWriter e w ()
tell msg = ReaderWriter $ \_ -> ((), msg)


tst :: ReaderWriter [String] [String] String
tst = do 
    h <- asks head
    tell ["kek"]
    return h

runTst = runRW tst ["1", "2", "3"]