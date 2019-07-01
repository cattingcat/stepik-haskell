module HsFp2.MonadsAndEffects.TripleNestedTrans where
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Data.Char
import Data.String
    
type MyRW = ReaderT [String] (Writer String) 

type MyRWT m a = ReaderT [String] (WriterT String m) a

-- [string] -> IO (a, String) 

runMyRWT :: MyRWT m a -> [String] -> m (a, String)
runMyRWT rwt e = runWriterT (runReaderT rwt e)

myAsks :: Monad m => ([String] -> a) -> MyRWT m a
myAsks f = reader f

myAsk :: Monad m => MyRWT m [String]
myAsk = myAsks id

myTell :: Monad m => String -> MyRWT m ()
myTell msg = lift (tell msg)

myLift :: Monad m => m a -> MyRWT m a
myLift = lift . lift





logFirstAndRetSecond :: MyRWT IO String
logFirstAndRetSecond = do
  el1 <- myAsks head
  myLift $ putStrLn $ "First is " ++ show el1
  el2 <- myAsks (map toUpper . head . tail)
  myLift $ putStrLn $ "Second is " ++ show el2
  myTell el1
  return el2

tst1 = runMyRWT logFirstAndRetSecond ["abc","defg","hij"]
-- First is "abc"
-- Second is "DEFG"
-- ("DEFG","abc")




logFirstAndRetSecondSafe :: MyRWT Maybe String
logFirstAndRetSecondSafe = do
  xs <- myAsk
  case xs of
    (f:s:_) -> myTell f >> return (map toUpper s)
    _ -> myLift Nothing


veryComplexComputation :: MyRWT Maybe (String, String)
veryComplexComputation = do
    lines <- myAsk
    let odds = filter (odd . length) lines
    let evens = filter (even . length) lines
    case (odds, evens) of 
        (fo:so:_, fe:se:_) -> do
            myTell (fe ++ "," ++ fo)
            return (fmap toUpper se, fmap toUpper so)
        _ -> myLift Nothing

tst21 = runMyRWT logFirstAndRetSecond ["abc","defg","hij"] -- Just ("DEFG","abc")
tst22 = runMyRWT logFirstAndRetSecond ["abc"] -- Nothing
tst23 = runMyRWT veryComplexComputation ["abc","defg","hij"] -- Nothing
tst24 = runMyRWT veryComplexComputation ["abc","defg","hij","kl"] -- Just (("KL","HIJ"),"defg,abc")