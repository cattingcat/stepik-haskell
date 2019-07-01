module HsFp2.MonadsAndEffects.StateTTasks.TreeTask where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Data.Monoid
import Data.Char

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)


numberAndCount :: Tree () -> (Tree Integer, Integer)
numberAndCount t = getSum <$> runWriter (evalStateT (go t) 1)

tst11 = numberAndCount (Leaf ()) -- (Leaf 1,1)
tst12 = numberAndCount (Fork (Leaf ()) () (Leaf ())) -- (Fork (Leaf 1) 2 (Leaf 3),2)



go :: Tree () -> StateT Integer (Writer (Sum Integer)) (Tree Integer)
go (Leaf _) = do 
    lift $ tell 1
    index <- get
    put (index + 1)
    return (Leaf index)
go (Fork l v r) = do
    leftBr <- go l
    index <- get
    put (index + 1)
    rightBr <- go r
    return (Fork leftBr index rightBr)
    


tree = Fork 
    (Leaf ()) 
    () 
    (Leaf ())


tst21 = (runStateT (go tree) 1)

