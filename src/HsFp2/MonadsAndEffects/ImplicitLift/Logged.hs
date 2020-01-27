{-# LANGUAGE InstanceSigs #-}  -- allows signatures in instance declaration
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- only mutliparam classes
{-# LANGUAGE FunctionalDependencies #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE UndecidableInstances #-} 


module HsFp2.MonadsAndEffects.ImplicitLift.Logged where
import Data.Monoid
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Trans
import qualified Control.Monad.Trans.State as S
import qualified Control.Monad.Trans.Reader as R
import Control.Monad.State.Class
import Control.Monad.Reader.Class


data Logged a = Logged String a deriving (Eq,Show)

newtype LoggT m a = LoggT { runLoggT :: m (Logged a) }

instance Monad m => Monad (LoggT m) where
    return x = LoggT $ return (Logged "" x)
    m >>= k  = LoggT $ do
        (Logged s  a) <- runLoggT m
        (Logged s' b) <- runLoggT (k a)
        return $ Logged (s ++ s') b
--    fail msg = LoggT $ fail msg

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


instance MonadState s m => MonadState s (LoggT m) where
    get   = lift get
    put   s = lift $ put s
    state f = lift $ state f

instance MonadReader r m => MonadReader r (LoggT m) where
    ask    = lift ask
    local  f = mapLoggT (\stm -> local f stm)
    reader f = lift $ reader f

mapLoggT :: (m (Logged a) -> n (Logged b)) -> LoggT m a -> LoggT n b
mapLoggT f = LoggT . f . runLoggT






class Monad m => MonadLogg m where
    w2log :: String -> m ()
    logg :: Logged a -> m a
  
instance Monad m => MonadLogg (LoggT m) where
    w2log = write2log
    logg  = LoggT . return

instance MonadLogg m => MonadLogg (S.StateT s m) where
    w2log s = lift $ w2log s
    logg  l = lift $ logg l

instance MonadLogg m => MonadLogg (R.ReaderT r m) where
    w2log s = lift $ w2log s
    logg  l = lift $ logg l











logSt' :: LoggT (S.State Integer) Integer      
logSt' = do 
  modify (+1)                   -- no lift!
  a <- get                      -- no lift!
  write2log $ show $ a * 10
  put 42                        -- no lift!
  return $ a * 100


tst1 = S.runState (runLoggT logSt') 2 -- (Logged "30" 300,42)






-- logRdr :: LoggT (R.Reader [(Int,String)]) ()      
-- logRdr = do 
--   Just x <- asks $ lookup 2                      -- no lift!
--   write2log x
--   Just y <- local ((3,"Jim"):) $ asks $ lookup 3 -- no lift!
--   write2log y

-- tst2 = R.runReader (runLoggT logRdr) [(1,"John"),(2,"Jane")] -- Logged "JaneJim" ()