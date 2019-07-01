module HsFp2.PrsEP where
import Control.Applicative

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

instance Functor PrsEP where
    fmap f (PrsEP prs) = PrsEP parse where
        parse ind str = case prs ind str of 
            (i, Left s)         -> (i, Left s)
            (i, Right (a, s))   -> (i, Right (f a, s))

instance Applicative PrsEP where 
    pure a =  PrsEP $ \i s -> (i, Right (a, s))
    (<*>) (PrsEP fprs) (PrsEP vprs) = PrsEP parse where
        parse ind str = case fprs ind str of 
            (i, Left s)         -> (i, Left s)
            (i, Right (f, s))   -> case vprs i s of
                (i', Left s')       -> (i', Left s')
                (i', Right (v, s')) -> (i', Right (f v, s'))

instance Alternative PrsEP where 
    empty = PrsEP $ \_ _ -> (0, Left $ fmtErrMsg 0 "empty alternative")
    (<|>) (PrsEP aprs) (PrsEP bprs) = PrsEP parse where
        parse ind str = case aprs ind str of 
            a@(i, Left s) -> case bprs ind str of 
                b@(i', Left s') -> if i > i' then a else b
                b -> b
            a -> a


parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP p = PrsEP parse where
    parse ind "" = let i = ind + 1 in (i, Left $ fmtErrMsg i "unexpected end of input")
    parse ind (c:str) | p c = (ind + 1, Right (c, str))
                      | otherwise = let i = ind + 1 in (i, Left $ fmtErrMsg i "unexpected " ++ c:[])

charEP :: Char -> PrsEP Char
charEP c = satisfyEP (== c)

anyEP :: PrsEP Char
anyEP = satisfyEP (const True)

testP = (,) <$> anyEP <* charEP 'B' <*> anyEP


fmtErrMsg :: Int -> String -> String
fmtErrMsg ind msg = "pos " ++ show ind ++ ": " ++ msg


tst11 = parseEP (charEP 'A') "ABC" -- Right ('A',"BC")
tst12 = parseEP (charEP 'A') "BCD" -- Left "pos 1: unexpected B"
tst13 = parseEP (charEP 'A') ""    --Left "pos 1: unexpected end of input"


tst21 = runPrsEP (pure 42) 0 "ABCDEFG" -- (0,Right (42,"ABCDEFG"))
tst22 = runPrsEP testP 0 "ABCDE" -- (3,Right (('A','C'),"DE"))
tst23 = parseEP testP "BCDE" -- Left "pos 2: unexpected C"
tst24 = parseEP testP "" -- Left "pos 1: unexpected end of input"
tst25 = parseEP testP "B" -- Left "pos 2: unexpected end of input"



tripleP [a,b,c] = (\x y z -> [x,y,z]) <$> charEP a <*> charEP b <*>  charEP c

tst31 = runPrsEP empty 0 "ABCDEFG" -- (0,Left "pos 0: empty alternative")
tst32 = parseEP (tripleP "ABC" <|> tripleP "ADC") "ABE" -- Left "pos 3: unexpected E"
tst33 = parseEP (tripleP "ABC" <|> tripleP "ADC") "ADE" -- Left "pos 3: unexpected E"
tst34 = parseEP (tripleP "ABC" <|> tripleP "ADC") "AEF" -- Left "pos 2: unexpected E"