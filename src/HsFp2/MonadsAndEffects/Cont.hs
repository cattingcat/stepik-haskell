module HsFp2.MonadsAndEffects.Cont where
import Debug.Trace

type Checkpointed a = (a -> Cont a a) -> Cont a a

runCheckpointed :: Show a => (a -> Bool) -> Checkpointed a -> a
runCheckpointed p check = let 
    cont = check (\x -> Cont (\c -> let nxt = c x in if p nxt then nxt else x))
    in runCont cont id


addTens :: Int -> Checkpointed Int
addTens x1 = \checkpoint -> do
  checkpoint x1
  let x2 = x1 + 10
  checkpoint x2     {- x2 = x1 + 10 -}
  let x3 = x2 + 10
  checkpoint x3     {- x3 = x1 + 20 -}
  let x4 = x3 + 10
  return x4         {- x4 = x1 + 30 -}


tst11 =  runCheckpointed (< 100) $ addTens 1 -- 31
tst12 =  runCheckpointed  (< 30) $ addTens 1 -- 21
tst13 =  runCheckpointed  (< 20) $ addTens 1 -- 11
tst14 =  runCheckpointed  (< 10) $ addTens 1 -- 1




newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where 
    fmap f (Cont c) = Cont (\cr -> c (\a -> cr (f a)))

instance Applicative (Cont r) where
    pure a = Cont  (\f -> f a)
    (<*>) (Cont af) (Cont av) = Cont (\ff -> af (\f -> av (\v -> ff (f v))))

instance Monad (Cont r) where
    return = pure
    (>>=) (Cont mv) f = Cont (\fb -> mv (\v -> runCont (f v) fb))

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a 
callCC f = Cont $ \c -> runCont (f (\a -> Cont $ \_ -> c a) ) c

cccTst x = callCC $ \cb -> do
    a <- return 1
    b <- return x
    c <- if a + b > 5 then cb 555 else return $ a + b
    return c

runCccTst x = runCont (cccTst x) id



continue :: a -> Cont a a 
continue a = Cont (\c -> c a)

constIdCont :: a -> Cont a a
constIdCont a = Cont (\c -> a)

evalCont :: Cont r r -> r
evalCont c = runCont c id

square' :: Int -> Cont r Int
square' n = Cont (\f -> f (n^2))

sum' :: Int -> Int -> Cont r Int
sum' a b = Cont (\f -> f (a+b))

sumSquares' :: Int -> Int -> Cont r Int 
sumSquares' a b = do
    sa <- square' a
    sb <- square' b
    c  <- sum' sa sb 
    return c

ssr = runCont (sumSquares' 3 4) id

kekPuk :: Cont Int Int
kekPuk = do
    Cont $ \c -> c 55
    a <- Cont $ \c -> c 3
    return a
    
kpt = runCont kekPuk id