module HsFp2.MonadsAndEffects.ValidateSum where
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Except

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


newtype Validate e a = Validate { getValidate :: Either [e] a }

instance Functor (Validate e) where
    fmap = liftM

instance Applicative (Validate e) where
    pure = return
    (<*>) = ap

instance Monad (Validate e) where
    return = Validate . Right 
    (>>=) (Validate (Left es)) _ = Validate $ Left es
    (>>=) (Validate (Right a)) f = f a

instance Alternative (Validate e) where
    empty = Validate $ Left []
    (<|>) (Validate (Left esa)) (Validate (Left esb)) = Validate $ Left (esa ++ esb)
    (<|>) (Validate (Left _)) b = b
    (<|>) a _ = a

instance MonadPlus (Validate e) where
    mzero = Validate $ Left []
    mplus (Validate (Left esa)) (Validate (Left esb)) = Validate $ Left (esa ++ esb)
    mplus (Validate (Left _)) b = b
    mplus a _ = a

instance Monoid a => Semigroup (Validate e a) where 
    (<>) = mappend

instance Monoid a => Monoid (Validate e a) where
    mempty = Validate $ Right mempty
    mappend (Validate (Left esa)) (Validate (Left esb)) = Validate $ Left (esa ++ esb)
    mappend (Validate (Right a)) (Validate (Right b)) = Validate $ Right (a <> b)
    mappend a@(Validate (Left _)) _ = a
    mappend _ b@(Validate (Left _)) = b

instance Semigroup Integer where 
    (<>) = (+)
    
instance Monoid Integer where
    mempty = 0
    mappend = (+)
    

collectE :: Except e a -> Validate e a
collectE = Validate . either (Left . (:[])) Right . runExcept


validateSum :: [String] -> Validate SumError Integer
validateSum ss = let 
    reads = fmap tryRead ss -- :: [Except ReadError Integer]
    zips = reads `zip` [1 ..] 
    sumErrs = fmap (\(r, ind) -> (SumError ind) `withExcept` r) zips
    validates = fmap collectE sumErrs
    in mconcat validates
    --in msum validates




tst11 = getValidate $ validateSum ["10", "20", "30"] -- Right 60
tst12 = getValidate $ validateSum ["10", "", "30", "oops"] -- Left [SumError 2 EmptyInput,SumError 4 (NoParse "oops")]