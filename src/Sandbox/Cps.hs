module HsFp2.MonadsAndEffects.Cps where
-- CPS - Continuation Passing Style

import Control.Monad
import Data.Foldable
import Debug.Trace

addO :: Int -> Int -> Int 
addO a b = a + b

add :: Int -> Int -> (Int -> r) -> r
add a b c = c $ a + b 

square :: Int -> (Int -> r) -> r
square a c = c $ a ^ 2

sumSquares :: Int -> Int -> (Int -> r) -> r
sumSquares a b c = 
    square a $ \a2 ->
    square b $ \b2 ->
    add a2 b2 $ \s -> 
    c s

tst1 = sumSquares 3 4 id

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

evalCont :: Cont r r -> r
evalCont c = runCont c id

instance Functor (Cont r) where 
    fmap f (Cont cont) = Cont $ \c -> cont (\c' -> c (f c'))

instance Applicative (Cont r) where 
    pure a = Cont $ \c -> c a
    (<*>) (Cont cf) (Cont cv) = Cont $ \c -> (cf $ \f -> (cv $ \v -> c (f v) ))

instance Monad (Cont r) where 
    return = pure
    (Cont ca) >>= f = Cont $ \c -> (ca $ \a -> (runCont (f a) $ \b -> c b))

-- Allows to break calculations in any moment (see tst x)
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \c -> runCont (f (\a -> Cont $ \_ -> c a) ) c


-- when' :: Applicative m => Bool -> m () -> m ()
-- when' True m = m
-- when' False _ = pure ()

tst x = callCC $ \break -> do 
    a <- return x
    b <- return 99
    when (a > b) (break 1000)
    return $ a + b

runTst x = runCont (tst x) id

tst11 = runTst 4 -- 103
tst12 = runTst 44 -- 143
tst13 = runTst 444 -- 1000
