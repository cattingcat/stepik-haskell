module HsFp2.MonadsAndEffects.ExceptT.TreeTask where
import Control.Monad
import Data.Foldable
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Data.Monoid
import Data.Char

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

instance Foldable Tree where
    foldr f ini (Leaf v) = f v ini
    foldr f ini (Fork l v r) = let fr = f v (foldr f ini r) in (foldr f fr l) 


data ReadError = EmptyInput | NoParse String deriving Show

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead "" = throwE $ EmptyInput
tryRead str = case readsPrec 0 str of 
    []           -> throwE $ NoParse str
    ((a, ""):[]) -> return a
    _            -> throwE $ NoParse str


go :: String -> ExceptT ReadError (Writer (Sum Integer)) ()
go s = do
    v <- tryRead s :: ExceptT ReadError (Writer (Sum Integer)) Integer
    lift $ tell (Sum v)
    return ()

treeSum t = let 
    (err, s) = runWriter . runExceptT $ traverse_ go t
    in (maybeErr err, getSum s) where
        maybeErr :: Either ReadError () -> Maybe ReadError
        maybeErr = either Just (const Nothing)


tst11 = treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "oops")) "15" (Leaf "16") -- (Just (NoParse "oops"),3)
tst12 = treeSum $ Fork (Fork (Leaf "1") "2" (Leaf "0")) "15" (Leaf "16") -- (Nothing,34)