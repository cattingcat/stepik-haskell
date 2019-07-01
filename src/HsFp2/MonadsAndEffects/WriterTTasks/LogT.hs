{-# LANGUAGE InstanceSigs #-}  -- allows signatures in instance declaration
{-# LANGUAGE KindSignatures #-}

module HsFp2.MonadsAndEffects.WriterTTasks.LogT where
import Data.Monoid
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State


data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Monad (LoggT m) where
    return x = LoggT $ return (Logged "" x)
    m >>= k  = LoggT $ do
        (Logged s a) <- runLoggT m
        (Logged s' b) <- runLoggT (k a)
        return $ Logged (s ++ s') b
    fail msg = LoggT $ fail msg

instance Monad m => Applicative (LoggT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (LoggT m) where
    fmap = liftM

instance MonadTrans LoggT where 
    lift :: Monad m => m a -> LoggT m a
    lift = LoggT . fmap (Logged "")


write2log :: Monad m => String -> LoggT m ()
write2log s = LoggT $ return (Logged s ())

type Logg = LoggT Identity

runLogg :: Logg a -> Logged a
runLogg lg = runIdentity (runLoggT lg)


-- logTst :: LoggT Identity Integer
-- logTst = do 
--     x <- LoggT $ Identity $ Logged "AAA" 30
--     y <- return 10
--     z <- LoggT $ Identity $ Logged "BBB" 2
--     return $ x + y + z
    
-- failTst :: [Integer] -> LoggT [] Integer
-- failTst xs = do
--     5 <- LoggT $ fmap (Logged "") xs
--     LoggT [Logged "A" ()]
--     return 42

-- tst11 = runIdentity (runLoggT logTst) -- Logged "AAABBB" 42
-- tst12 = runLoggT $ failTst [5,5] -- [Logged "A" 42,Logged "A" 42]
-- tst13 = runLoggT $ failTst [5,6] -- [Logged "A" 42]
-- tst14 = runLoggT $ failTst [7,6] -- []

logTst' :: Logg Integer   
logTst' = do 
  write2log "AAA"
  write2log "BBB"
  return 42
-- runLogg logTst'


logSt :: LoggT (State Integer) Integer
logSt = do 
  lift $ modify (+1)
  a <- lift get
  write2log $ show $ a * 10
  lift $ put 42
  return $ a * 100

logStTst = runState (runLoggT logSt) 2