{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}




module Sandbox.MaybeT where


newtype MaybeT (m :: * -> *) (a :: *) = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT ma) = MaybeT $ fmap (fmap f) ma

instance (Applicative m) => Applicative (MaybeT m) where 
    pure :: a -> MaybeT m a
    pure = MaybeT . pure . Just

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (<*>) (MaybeT mf) (MaybeT ma) = MaybeT $ foo <$> mf <*> ma where
        foo :: Maybe (a -> b) -> Maybe a -> Maybe b
        foo mf ma = mf <*> ma

instance (Monad m) => Monad (MaybeT m) where 
    return :: a -> MaybeT m a
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (>>=) (MaybeT ma) f = MaybeT $ ma >>= foo where 
        -- foo :: Maybe a -> m (Maybe b)
        foo (Just a) = runMaybeT $ f a
        foo _        = return Nothing

-- instance (Monad m) => Applicative (MaybeT m) where 
--     pure :: a -> MaybeT m a
--     pure = MaybeT . pure . Just

--     (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
--     (<*>) (MaybeT mf) (MaybeT ma) = MaybeT $ do
--         maybeF <- mf
--         maybeA <- ma
--         return $ maybeF <*> maybeA




-- see: ScopedTypeVariables 

kekPuk :: (a -> b) -> a -> b
kekPuk (f :: a -> b) = foo where 
    foo :: a -> b
    foo a = f a


kekPuk' :: forall a b. (a -> b) -> a -> b
kekPuk' f = foo where 
    foo :: a -> b
    foo a = f a