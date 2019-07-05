module ParsePerson where

import Data.String
import Data.List(all)
import Data.Text(pack, unpack, splitOn)
import Text.Read (readMaybe)

data Error = ParsingError | IncompleteDataError | IncorrectDataError String deriving (Show)

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show)

parsePerson :: String -> Either Error Person
parsePerson s = let 
    l = splitOn (pack "\n") (pack s)
    d = map (splitOn (pack " = ")) l

    isValid (k:v:[]) = True
    isValid _        = False

    value n = loop d where
        loop [] = Nothing
        loop ((name:val:[]):ds) = if name == pack n then Just (unpack val) else loop ds

    personInfo = case (value "firstName", value "lastName", value "age") of 
        (Just a, Just b, Just c) -> Just (a,b,c)
        _                        -> Nothing

    getPerson = case personInfo of
        Nothing             -> Left $ IncompleteDataError
        Just (fn, ln, age)  -> case readMaybe age :: Maybe Int of
            Nothing     -> Left $ IncorrectDataError age
            Just ageInt -> Right $ Person fn ln ageInt

    parse | not $ all isValid d = Left $ ParsingError
          | otherwise = getPerson

    in parse




test1 = "firstName = John\nlastName = Connor\nage = 30"
splitTest = splitOn (pack "\n") (pack test1)

dataTest = fmap (splitOn (pack " = ")) splitTest