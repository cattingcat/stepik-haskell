module HsFp2.DefaultImplsTree where
import Data.Traversable (foldMapDefault, Traversable(..))

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Foldable Tree where
    foldMap = foldMapDefault

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch l v r) = Branch (fmap f l) (f v) (fmap f r)

instance Traversable Tree where
    sequenceA Nil = pure Nil    
    sequenceA (Branch l v r) = (\l -> flip (Branch l)) <$> sequenceA l <*> sequenceA r <*> v 



testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
test11 = foldMapDefault (\x -> [x]) testTree -- [1,3,2,5,4]