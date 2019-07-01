{-# LANGUAGE InstanceSigs #-} 

module HsFp2.MonadsAndEffects.ReaderTTasks.Arrs where
import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.Identity

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

newtype Arr2T e1 e2 m a = Arr2T { getArr2T :: e1 -> e2 -> m a }
newtype Arr3T e1 e2 e3 m a = Arr3T { getArr3T :: e1 -> e2 -> e3 -> m a }

arr2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
arr2 f = Arr2T $ \a b -> return (f a b)

arr3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
arr3 f = Arr3T $ \a b c -> return (f a b c)

instance (Functor m) => Functor (Arr2T e1 e2 m) where 
    fmap :: (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
    fmap f (Arr2T fv) = Arr2T $ \e1 e2 -> fmap f (fv e1 e2)

instance (Functor m) => Functor (Arr3T e1 e2 e3 m) where 
    fmap :: (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
    fmap f (Arr3T fv) = Arr3T $ \e1 e2 e3 -> fmap f (fv e1 e2 e3)

instance (Applicative m) => Applicative (Arr2T e1 e2 m) where 
    pure a = Arr2T $ \_ _ -> pure a
    (<*>) :: Arr2T e1 e2 m (a -> b) -> Arr2T e1 e2 m a -> Arr2T e1 e2 m b
    (Arr2T f) <*> (Arr2T v) = Arr2T $ \e1 e2 -> (f e1 e2) <*> (v e1 e2)

instance (Applicative m) => Applicative (Arr3T e1 e2 e3 m) where 
    pure a = Arr3T $ \_ _ _ -> pure a
    (<*>) :: Arr3T e1 e2 e3 m (a -> b) -> Arr3T e1 e2 e3 m a -> Arr3T e1 e2 e3 m b
    (Arr3T f) <*> (Arr3T v) = Arr3T $ \e1 e2 e3 -> (f e1 e2 e3) <*> (v e1 e2 e3)


instance (Monad m) => Monad (Arr2T e1 e2 m) where 
    return = pure
    (>>=) :: Arr2T e1 e2 m a -> (a -> Arr2T e1 e2 m b) -> Arr2T e1 e2 m b
    (Arr2T va) >>= f = Arr2T $ \e1 e2 -> do 
        v <- va e1 e2
        let (Arr2T vb) = f v
        vb e1 e2

instance (Monad m) => Monad (Arr3T e1 e2 e3 m) where 
    return = pure
    fail :: String -> Arr3T e1 e2 e3 m a
    fail s = Arr3T $ \_ _ _ -> fail s
    (>>=) :: Arr3T e1 e2 e3 m a -> (a -> Arr3T e1 e2 e3 m b) -> Arr3T e1 e2 e3 m b
    (Arr3T va) >>= f = Arr3T $ \e1 e2 e3 -> do 
        v <- va e1 e2 e3
        let (Arr3T vb) = f v
        vb e1 e2 e3

instance MonadTrans (Arr2T e1 e2) where 
    lift :: Monad m => m a -> Arr2T e1 e2 m a
    lift ma = Arr2T $ \_ _ -> ma

instance MonadTrans (Arr3T e1 e2 e3) where 
    lift :: Monad m => m a -> Arr3T e1 e2 e3 m a
    lift ma = Arr3T $ \_ _ _ -> ma

asks2 :: Monad m => (e1 -> e2 -> a) -> Arr2T e1 e2 m a
asks2 f = Arr2T $ \e1 e2 -> return (f e1 e2)

asks3 :: Monad m => (e1 -> e2 -> e3 -> a) -> Arr3T e1 e2 e3 m a
asks3 f = Arr3T $ \e1 e2 e3 -> return (f e1 e2 e3)


tst11 = (getArr2T $ arr2 (+)) 33 9 :: [Integer] -- [42]
tst12 = (getArr3T $ arr3 foldr) (*) 1 [1..5] :: Either String Integer -- Right 120
tst13 = runIdentity $ (getArr2T $ arr2 (+)) 33 9 -- 42


a2l = Arr2T $ \e1 e2 -> [e1,e2,e1+e2]
a3e = Arr3T $ \e1 e2 e3 -> Right (e1+e2+e3)

tst21 = (getArr2T $ succ <$> a2l) 10 100 -- [11,101,111]
tst22 = (getArr3T $ sqrt <$> a3e) 2 3 4 -- Right 3.0


a2l' = Arr2T $ \e1 e2 -> [e1,e2]
a2fl = Arr2T $ \e1 e2 -> [(e1*e2+),const 7]
a3fl = Arr3T $ \e1 e2 e3 -> [(e2+),(e3+)]
a3l = Arr3T $ \e1 e2 e3 -> [e1,e2]

tst31 = getArr2T (a2fl <*> a2l') 2 10 -- [22,30,7,7]
tst32 = getArr3T (a3fl <*> a3l) 3 5 7 -- [8,10,10,12]


a2l'' = Arr2T $ \e1 e2 -> [e1,e2]
a3m = Arr3T $ \e1 e2 e3 -> Just (e1 + e2 + e3)

tst41 = getArr2T (do {x <- a2l''; y <- a2l''; return (x + y)}) 3 5 -- [6,8,8,10]
tst42 = getArr3T (do {x <- a3m; y <- a3m; return (x * y)}) 2 3 4 -- Just 81

tst51Arr2T :: Arr2T a b Identity (a, b, (a, b))
tst51Arr2T = do 
    x <- asks2 const        -- const :: a -> b -> a 
    y <- asks2 (flip const) -- flip const :: b -> a -> a
    z <- asks2 (,)          -- (,) :: a -> b -> (a,b)
    return (x,y,z)


tst51 = getArr2T tst51Arr2T 'A' 'B'    -- ('A','B',('A','B'))