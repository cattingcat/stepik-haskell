module HsFp2.MyParser where

import Control.Applicative
import Data.Char

newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }


instance Functor Prs where
    fmap f (Prs fa) = Prs parse where
        parse str = fmap (\(a,s) -> (f a, s)) (fa str)
    
instance Applicative Prs where
    pure a = Prs (\s -> Just (a,s))
    (<*>) (Prs pf) (Prs pv) = Prs parse where
        parse str = case pf str of
            Nothing     -> Nothing 
            Just (f, s) -> case pv s of
                Nothing      -> Nothing 
                Just (v, s') -> Just (f v, s')

instance Alternative Prs where
    empty = Prs $ const Nothing
    (<|>) (Prs pf1) (Prs pf2) = Prs parse where
        parse str = case pf1 str of
            r@(Just _) -> r
            Nothing -> pf2 str


anyChr :: Prs Char
anyChr = Prs parse where
    parse "" = Nothing
    parse (s:ss) = Just (s, ss)

satisfy :: (Char -> Bool) -> Prs Char
satisfy p = Prs parse where
    parse "" = Nothing
    parse (s:ss) | p s = Just (s, ss)
                 | otherwise = Nothing

char :: Char -> Prs Char
char c = satisfy (==c)

nat :: Prs Int
nat = fmap (\s -> read s :: Int) (many1' $ satisfy isDigit)

many' :: Prs a -> Prs [a]
many' p = (:) <$> p <*> many' p <|> pure [] -- returns [] in rec call

many1' :: Prs a -> Prs [a]
many1' p = (:) <$> p <*> many' p

mult :: Prs Int
mult = (*) <$> nat <* char '*' <*> nat


test11 = runPrs anyChr "asd"
test12 = runPrs ((,) <$> anyChr <*> anyChr) "asd"
test13 = runPrs (anyChr *> anyChr) "asd"
test14 = runPrs (char 'a' <|> char 'A') "Asd"
test15 = runPrs (many' (char 'A')) "AAAB"
test151 = runPrs (many' (char 'A')) "QWE"
test16 = runPrs (many1' (char 'A')) "AAAB"
test17 = runPrs (many1' (char 'A')) "CDB"
test18 = runPrs (nat) "123asd"
test19 = runPrs mult "2*77AAA"





newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

instance Functor PrsE where
    fmap f (PrsE fp) = PrsE parse where
        parse str = fmap (\(a,s) -> (f a,s)) (fp str)

instance Applicative PrsE where
    pure a = PrsE (\s -> Right (a,s))
    (<*>) (PrsE pf) (PrsE pv) = PrsE parse where
        parse str = do
            (f, s') <- pf str
            (v, s'') <- pv s' 
            return (f v, s'')

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE parse where
    parse "" = Left "unexpected end of input"
    parse (s:ss) | p s = Right (s, ss)
                 | otherwise = Left $ "unexpected " ++ (s:[])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

anyE :: PrsE Char
anyE = satisfyE (const True)

test21 = runPrsE (charE 'A') "ABC"
test22 = runPrsE (charE 'A') "BCD"
test23 = runPrsE (charE 'A') ""
test24 = runPrsE ((,) <$> anyE <* charE '*' <*> charE 'C') "A*CD"