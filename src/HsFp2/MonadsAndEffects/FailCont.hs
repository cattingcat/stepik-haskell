module HsFp2.MonadsAndEffects.FailCont where
import Control.Monad.Trans.Except


newtype Cont r a = Cont { runCont :: (a -> r) -> r }
data ReadError = EmptyInput | Other deriving (Show, Eq)

newtype FailCont r e a = FailCont { runFailCont :: (a -> r) -> (e -> r) -> r }

instance Functor (FailCont r e) where 
    fmap f fv = FailCont $ \ok err -> let
        okCb a  = ok (f a)
        errCb e = err e
        in runFailCont fv okCb errCb

instance Applicative (FailCont r e) where
    pure a = FailCont $ \ok _ -> ok a
    (<*>) af av = FailCont $ mainCb where 
        mainCb ok err = runFailCont af okCb err where
            okCb f = runFailCont av okCb' err where 
                okCb' v = ok (f v)

instance Monad (FailCont r e) where 
    return = pure
    (>>=) mv f = FailCont $ mainCb where 
        mainCb ok err = runFailCont mv okCb err where 
            okCb v = runFailCont (f v) ok err

callCFC :: ((a -> FailCont r e b) -> FailCont r e a) -> FailCont r e a 
callCFC f = FailCont $ \ok err -> runFailCont (f (\a -> FailCont $ \_ _ -> ok a) ) ok err

toFailCont :: Except e a -> FailCont r e a
toFailCont e = case runExcept e of
    Left e  -> FailCont $ \_ er -> er e
    Right a -> FailCont $ \ok _ -> ok a

evalFailCont :: FailCont (Either e a) e a -> Either e a
evalFailCont fc = runFailCont fc Right Left

add :: Int -> Int -> FailCont r e Int
add x y = FailCont $ \ok _ -> ok $ x + y

-- addInts :: String -> String -> FailCont r ReadError Int
-- addInts s1 s2 = do
--   i1 <- toFailCont $ tryRead s1
--   i2 <- toFailCont $ tryRead s2
--   return $ i1 + i2


tst10 = runFailCont (add 3 5) id id
-- tst11 = evalFailCont $ addInts "15" "12" -- Right 27
-- tst12 = runFailCont (addInts "15" "") print (putStrLn . ("Oops: " ++) . show) -- Oops: EmptyInput