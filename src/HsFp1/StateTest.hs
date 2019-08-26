module HsFp1.StateTest where 
import Control.Monad.State (State, get, put, execState, modify', runState, evalState)
import Control.Monad (replicateM)

fibStep :: State (Integer, Integer) ()
fibStep = do
    (a,b) <- get
    put (b, a+b)

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState (replicateM n m)



    
-- system code
data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving Show

-- solution code
numberTree :: Tree () -> Tree Integer
numberTree tree = evalState (helper tree) 1 where
    helper (Leaf _) = do
        n <- get
        put (n+1)
        return $ Leaf n
    helper (Fork l _ r) = do
        l' <- helper l
        n <- get
        put (n+1)
        r' <- helper r
        return $ Fork l' n r'