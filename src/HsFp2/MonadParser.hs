module HsFp2.MonadParser where

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE p = PrsE parse where
    parse "" = Left "unexpected end of input"
    parse (s:ss) | p s = Right (s, ss)
                 | otherwise = Left $ "unexpected " ++ (s:[])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
    fmap f (PrsE v) = PrsE parse where
        parse s = case v s of
            Left err -> Left err
            Right (r, rest) -> Right (f r, rest)


instance Applicative PrsE where
    pure a = PrsE $ \s -> Right (a, s)
    (<*>) (PrsE af) (PrsE av) = PrsE parse where
        parse s = case af s of
            Left err -> Left err
            Right (f, rest) -> case av rest of
                Left err -> Left err
                Right (v, rest') -> Right (f v, rest')

instance Monad PrsE where 
    (>>=) (PrsE ma) f = PrsE parse where
        parse s = case ma s of
            Left err -> Left err
            Right (r, rest) -> let (PrsE p) = f r in p rest


p11 = do 
    a <- charE 'A'
    b <- charE 'B'
    return (a,b)

test11 = runPrsE p11 "ABC"
test12 = runPrsE p11 "ACD"
test13 = runPrsE p11 "ACD"

