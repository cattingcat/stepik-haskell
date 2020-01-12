module HsFp2.MonadsAndEffects.ExceptMonad where
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Except
import Text.Read
import Data.Foldable (msum)

newtype MyExcept e a = MyExcept { runMyExcept :: Either e a }

instance Functor (MyExcept e) where
    fmap = liftM

instance Applicative (MyExcept e) where
    pure = return
    (<*>) = ap

instance Monad (MyExcept e) where
    return a = MyExcept $ Right a
    (>>=) ma f = case runMyExcept ma of
        Right a -> f a
        Left e  -> MyExcept $ Left e

withMyExcept :: (e -> e') -> MyExcept e a -> MyExcept e' a
withMyExcept f (MyExcept (Left e))  = MyExcept $ Left (f e)
withMyExcept _ (MyExcept (Right a)) = MyExcept $ Right a


data ListIndexError = ErrIndexTooLarge Int | ErrNegativeIndex deriving (Eq, Show)

newtype SimpleError = Simple { getSimple :: String } deriving (Eq, Show)

instance Semigroup SimpleError where
    (<>) = mappend

instance Monoid SimpleError where
    mempty = Simple ""
    mappend (Simple a) (Simple b) = Simple (a ++ b) 

lie2se :: ListIndexError -> SimpleError
lie2se ErrNegativeIndex = Simple "[negative index]"
lie2se (ErrIndexTooLarge i) = Simple $ "[index (" ++ show i ++ ") is too large]"

infixl 9 !!!
(!!!) :: [a] -> Int -> Except ListIndexError a
(!!!) _ ind | ind < 0 = throwE $ ErrNegativeIndex
(!!!) as ind = loop as ind where
    loop [] i | i >= 0 = throwE $ ErrIndexTooLarge ind
    loop (a:as) 0 = return a
    loop (a:as) n = loop as (n - 1)




data ReadError = EmptyInput | NoParse String deriving Show

tryRead :: Read a => String -> Except ReadError a
tryRead "" = throwE $ EmptyInput
tryRead str = case readsPrec 0 str of 
    []           -> throwE $ NoParse str
    ((a, ""):[]) -> return a
    _            -> throwE $ NoParse str


data SumError = SumError Int ReadError deriving Show

trySum :: [String] -> Except SumError Integer
trySum ss = loop (fmap tryRead ss) 1 0 where
    loop [] _ acc = return acc
    loop (a:as) ind acc = case runExcept a of
        Left e -> throwE $ SumError ind e
        Right v -> loop as (ind+1) (acc+v)

tst21 = runExcept (tryRead "5" :: Except ReadError Int) -- Right 5
tst22 = runExcept (tryRead "5" :: Except ReadError Double) -- Right 5.0
tst23 = runExcept (tryRead "5zzz" :: Except ReadError Int) -- Left (NoParse "5zzz")
tst24 = runExcept (tryRead "(True, ())" :: Except ReadError (Bool, ())) -- Right (True,())
tst25 = runExcept (tryRead "" :: Except ReadError (Bool, ())) -- Left EmptyInput
tst26 = runExcept (tryRead "wrong" :: Except ReadError (Bool, ())) -- Left (NoParse "wrong")

tst31 = runExcept $ trySum ["10", "20", "30"] -- Right 60
tst32 = runExcept $ trySum ["10", "20", ""] -- Left (SumError 3 EmptyInput)
tst33 = runExcept $ trySum ["10", "two", "30"] -- Left (SumError 2 (NoParse "two"))

-- either :: (a -> c) -> (b -> c) -> Either a b -> c


toSimple = runExcept . withExcept lie2se
xs = [1,2,3]

tst41 = toSimple $ xs !!! 42 -- Left (Simple {getSimple = "[index (42) is too large]"})
tst42 = toSimple $ xs !!! (-2) -- Left (Simple {getSimple = "[negative index]"})
tst43 = toSimple $ xs !!! 2 -- Right 3

toSimpleFromList = runExcept . msum . map (withExcept lie2se)

tst45 = toSimpleFromList [xs !!! (-2), xs !!! 42] -- Left (Simple {getSimple = "[negative index][index (42) is too large]"})
tst46 = toSimpleFromList [xs !!! (-2), xs !!! 2] -- Right 3