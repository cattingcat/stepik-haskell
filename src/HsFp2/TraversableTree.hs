module HsFp2.TraversableTree where
import Data.Monoid

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

instance Functor Tree where
    fmap f Nil            = Nil
    fmap f (Branch l v r) = Branch (fmap f l) (f v) (fmap f r)

instance Applicative Tree where 
    pure a = Branch Nil a Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Branch fl f fr) (Branch l v r) = Branch (fl <*> l) (f v) (fr <*> r)

instance Foldable Tree where 
    foldMap _ Nil = mempty
    foldMap f (Branch l v r) = let
        lv = foldMap f l
        rv = foldMap f r
        vv = f v in lv <> vv <> rv

instance Traversable Tree where
    -- :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    traverse _ Nil = pure Nil
    traverse f (Branch l v r) = let
        ftL = traverse f l
        ftR = traverse f r
        ftV = f v 
        in Branch <$> ftL <*> ftV <*> ftR