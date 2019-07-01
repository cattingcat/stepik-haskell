module HsFp2.MonadsAndEffects.WriterTTask where
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Data.Char
import Data.String


-- class MonadTrans (t :: (* -> *) -> * -> *) where
--     lift :: Monad m => m a -> t m a


logFirstAndRetSecond :: WriterT String (Reader [String]) String
-- logFirstAndRetSecond = WriterT $ asks (\env -> (map toUpper $ head env, head . tail $ env))
logFirstAndRetSecond = do
    fst <- lift $ asks head
    tell fst
    snd <- lift $ asks (map toUpper . head . tail)
    return snd

strings = ["dEFG", "abc", "sdfdfsdf"]
tst1 = runReader (runWriterT logFirstAndRetSecond) strings -- ("DEFG","abc")


------------------------------------------------------------------

separate :: (a -> Bool) -> (a -> Bool) -> [a] -> WriterT [a] (Writer [a]) [a]
separate p1 p2 l = do
    tell (filter p1 l)
    lift $ tell (filter p2 l)
    return (filter (\i -> not (p1 i || p2 i)) l)

sep2' :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a], [a])
sep2' p1 p2 l = f p1 p2 l [] [] [] where 
    f _ _ [] a1 a2 a3 = (a1,a2,a3)
    f p1 p2 (h:t) a1 a2 a3 = 
        if      p1 h then f p1 p2 t (h:a1) a2 a3 
        else if p2 h then f p1 p2 t a1 a2 (h:a3) 
        else              f p1 p2 t a1 (h:a2) a3


tst2 = (runWriter . runWriterT) $ separate (<3) (>7) [0..10] -- (([3,4,5,6,7],[0,1,2]),[8,9,10])



------------------------------------------------------------------

type MyRW = ReaderT [String] (Writer String) 

runMyRW :: MyRW a -> [String] -> (a, String)
runMyRW rw e = runWriter (runReaderT rw e)

myAsks :: ([String] -> a) -> MyRW a
myAsks = asks

myTell :: String -> MyRW ()
myTell = lift . tell

logFirstAndRetSecondMy :: MyRW String
logFirstAndRetSecondMy = do
  el1 <- myAsks head
  el2 <- myAsks (map toUpper . head . tail)
  myTell el1
  return el2

tst3 = runMyRW logFirstAndRetSecondMy strings