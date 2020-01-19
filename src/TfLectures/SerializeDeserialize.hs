module TfLectures.SerializeDeserialize where

import Prelude hiding ((^))
import Data.Char (isDigit)
import Text.Read (readMaybe)

-- | DSL examples:

ds1 :: FormattingSpec repr => repr a a
ds1 = lit "Hello world"

tp1 = sprintf $ ds1
ts1 = sscanf "Hello world" ds1 ()

ds2 :: FormattingSpec repr => repr a (Char -> a)
ds2 = lit "Hello " ^ lit "world" ^ char

tp2 = sprintf ds2 '!'
ts2 = sscanf "Hello world!" ds2 id


ds3 = lit "The value of " ^ char ^ lit " is " ^ int

ts3 :: Char -> Int -> String
ts3 = sprintf ds3


class FormattingSpec repr where
  lit  :: String -> repr a a
  int  :: repr a (Int -> a)
  char :: repr a (Char -> a)
  (^)  :: repr b c -> repr a b -> repr a c

infixl 5 ^



newtype FPr a b = FPr ((String -> a) -> b)

instance FormattingSpec FPr where
  lit s = FPr $ \k -> k s
  int   = FPr $ \k -> \x -> k (show x)
  char  = FPr $ \k -> \c -> k [c]
  (FPr a) ^ (FPr b) = FPr $ \k -> a (\sa -> b (\sb -> k (sa <> sb)))

sprintf :: FPr String b -> b
sprintf (FPr fmt) = fmt id



newtype FSc a b = FSc (String -> b -> Maybe (a, String))

instance FormattingSpec FSc where
  lit s = FSc $ \input b -> case strMatch input s of
    Just rest -> Just (b, rest)
    Nothing -> Nothing 
    
  int = FSc $ \input b -> 
    let 
      (prefix, rest) = span isDigit input
      num = if null prefix then Nothing else readMaybe prefix :: Maybe Int
      in case num of 
        Nothing -> Nothing
        Just n -> Just (b n, rest)
        
  char = FSc $ \input b -> case input of 
    [] -> Nothing 
    (c:cs) -> Just (b c, cs) 
    
  (FSc a) ^ (FSc b) = FSc $ \input f -> 
      (a input f) >>= (\(r, rest) -> b rest r)
   


sscanf :: String -> FSc a b -> b -> Maybe a
sscanf input (FSc fmt) f = fst <$> fmt input f


strMatch :: String -> String -> Maybe String
strMatch [] []    = Just []
strMatch [] inf   = Nothing
strMatch rest []  = Just rest
strMatch s@(c:cs) (k:ks) =
  if c == k
  then strMatch cs ks
  else Nothing
  
  
tst1 = sscanf "Aa 123 Bb c" (lit "Aa " ^ int ^ lit " Bb " ^ char) (\i c -> (i, c))