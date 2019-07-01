module Lib where
import Data.List
import Data.Char
import GHC.Base

someFunc :: IO ()
someFunc = putStrLn "someFunc sdf"


newtype Parser t r = Parser {runParser :: t -> Maybe (t, r)}

symbol :: Char -> Parser String Char
symbol c = Parser parse where
    parse (c':ss) | c' == c = Just (ss, c')
    parse _                 = Nothing

startW :: String -> Parser String String
startW s = Parser parse where 
    parse t | isPrefixOf s t = Just (drop (length s) t, s) 
    parse _                  = Nothing

digit :: Parser String Char
digit = Parser parse where
    parse (c:ss) | isDigit c = Just (ss, c)
    parse _                  = Nothing


digitS :: Parser String String
digitS = fmap (:[]) digit

multi :: Parser String String -> Parser String String
multi (Parser f) = Parser parse where
    parse str = case f str of
        Nothing -> Nothing
        Just (rest, res) -> Just $ loop (rest, res) where
            loop (rest, res) = case f rest of
                Nothing -> (rest, res) 
                Just (rest2, res2) -> loop (rest2, res ++ res2)

instance Functor (Parser t) where
    fmap f (Parser p) = Parser mapped where
        mapped t = fmap m $ p t where
            m (rest, res) = (rest, f res)

instance Applicative (Parser t) where
    pure a = Parser (\x -> Just (x, a))
    liftA2 g (Parser fa) (Parser fb) = Parser parse where
        parse tok = case fa tok of
            Nothing             -> Nothing
            Just  (rest1, res1) -> case fb rest1 of
                Nothing             -> Nothing
                Just  (rest2, res2) -> Just (rest2, g res1 res2)

foo :: Parser String Char
foo = symbol 'd'

foo1 :: Parser String Bool
foo1 = fmap (\s -> s == "A") $ startW "A"
 
foo3 :: Parser String String
foo3 = (++) <$> (startW "qwe") <*> (startW "asd")

foo4 :: Parser String String
foo4 = multi digitS