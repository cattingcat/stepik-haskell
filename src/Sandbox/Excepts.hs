module Sandbox.Excepts where 
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Text.Read

-- Maybe a    - we can't deal with errors, because it gives us only Nothind
-- Either l r - we can deal with errors, but it is too general 
--              Either can be used not only for errors
-- Except e a - special for errors (it contains Either inside)
--              and provides special methods for errors
--              throwE / catchE / either / except


data DivCalcError = DivByZero | FParseErr String | SParseErr String deriving (Show)

divCalc :: String -> String -> Except DivCalcError Double
divCalc a b = do 
    da <- tryParse a `catchE` c FParseErr
    db <- tryParse b `catchE` c SParseErr
    if db == 0.0 then throwE DivByZero 
    else return (da / db)
    where 
        c :: (String -> DivCalcError) -> ParseError -> Except DivCalcError a
        c ctor EmptyInput = throwE $ ctor "EmptyInput"
        c ctor (ParseError m) = throwE $ ctor ("ParseErr in: " ++ m)

textDiv :: String -> String -> String 
textDiv a b = either (show) (\r -> "Res: " ++ show r) (runExcept $ divCalc a b)

data ParseError = EmptyInput | ParseError String

tryParse :: Read a => String -> Except ParseError a
tryParse "" = throwE EmptyInput
tryParse s = case readsPrec 0 s of 
    ((a, ""):[]) -> return a
    _          -> throwE $ ParseError s


-- Also it is MonadPlus instance
tst1 :: Except String Int
tst1 = throwE "" `mplus` return 55

-- And Alternative
tst2 :: Except String Int
tst2 = throwE "" <|> return 55

tst3 :: Except String Int
tst3 = action `catchE` (\_ -> throwE "a > b error") where 
    action = do
        a <- return 5
        b <- return 7
        guard (a > b) ::Except String ()
        return $ a + b


tst4 = msum $ fmap (\ex -> ex `catchE` (throwE . show)) 
    [divCalc "1" "2", divCalc "3" "0", divCalc "423" "32", divCalc "4" "0"]

tst5 = msum $ fmap (\ex -> ex `catchE` (throwE . show)) 
    [divCalc "1" "0", divCalc "3" "0", divCalc "423" "0", divCalc "4" "0"]