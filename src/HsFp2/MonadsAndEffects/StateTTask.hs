module HsFp2.MonadsAndEffects.StateTTask where
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans


tickCollatz :: State Integer Integer
tickCollatz = do
    n <- get
    let res = if odd n then 3 * n + 1 else n `div` 2
    put res
    return n

type EsSi a = ExceptT String (State Integer) a

-- Except e a = Except (Either e a)
-- ExceptT e m a = ExceptT (m (Either e a))
-- State s a = State (s -> (a, s))
-- StateT s m a = State (s -> m (a, s))

-- EsSi a = ExceptT (State s (Either e a))
-- EsSi a = ExceptT (s -> ((Either e a), s) )


runEsSi :: EsSi a -> Integer -> (Either String a, Integer)
runEsSi essi n = runState (runExceptT essi) n


-- go :: Integer -> Integer -> State Integer Integer -> EsSi ()
-- go l h state = do
--     let mappedS = mapState (\(a, s) -> (s, s)) state
--     v <- lift mappedS
--     if v >= h then throwE "Upper bound"
--         else if v <= l then throwE "Lower bound"
--             else return ()

go :: Integer -> Integer -> State Integer Integer -> EsSi ()
go l h state = do
    lift state 
    currState <- lift get
    if currState >= h then throwE "Upper bound"
        else if currState <= l then throwE "Lower bound"
            else return ()


tst11 = runEsSi (go 1 85 tickCollatz) 27 -- (Right (),82)
tst12 = runEsSi (go 1 80 tickCollatz) 27 -- (Left "Upper bound",82)
tst13 = runEsSi (forever $ go 1 1000 tickCollatz) 27 -- (Left "Upper bound",1186)
tst14 = runEsSi (forever $ go 1 10000 tickCollatz) 27 -- (Left "Lower bound",1)


type RiiEsSiT m = ReaderT (Integer, Integer) (ExceptT String (StateT Integer m))

runRiiEsSiT :: ReaderT (Integer,Integer) (ExceptT String (StateT Integer m)) a 
                -> (Integer,Integer)  
                -> Integer 
                -> m (Either String a, Integer)
runRiiEsSiT x bounds@(l,h) is = runStateT (runExceptT (runReaderT x bounds)) is
    
go' :: Monad m => StateT Integer m Integer -> RiiEsSiT m ()
go' stm = do
    lift . lift $ stm
    (l,h) <- ask
    cs <- lift . lift $ get
    when (cs >= h) (lift $ throwE "Upper bound")
    when (cs <= l) (lift $ throwE "Lower bound")
    return ()


tickCollatz' :: StateT Integer IO Integer
tickCollatz' = do
    n <- get
    let res = if odd n then 3 * n + 1 else n `div` 2
    lift $ putStrLn $ show res
    put res
    return n

tst21 = runRiiEsSiT (forever $ go' tickCollatz') (1,200) 27
