module Tokenizer where
import Data.Char
import qualified Text.Read as T

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "" = Nothing
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace 
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken a = (T.readMaybe a :: Maybe Int) >>= return . Number

tokenize :: String -> Maybe [Token]
tokenize input = let
    ws = words input
    ts = fmap asToken ws 
    in upMaybe ts 

upMaybe :: [Maybe a] -> Maybe [a]
upMaybe [] = Just []
upMaybe (a:as) = do
    v <- a
    vs <- upMaybe as 
    return $ v:vs


test1 = tokenize "1 + ( 7 - 2 )"


pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple n | n <= 0 = []
                    | otherwise = do 
    x <- [1 .. n]
    y <- [1 .. x - 1]
    z <- [1 .. n]
    f <- if x^2 + y^2 == z^2 then [()] else []
    return (y,x,z)
    