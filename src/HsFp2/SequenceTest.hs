module HsFp2.SequenceTest where

import Data.Foldable

-- sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
-- sequenceA_ tfa = foldr (*>) (pure ()) tfa
-- execute all effects but drop value

test1 = sequenceA_ [Just 2, Just 3]
test2 = sequenceA_ [Just 2, Just 3, Nothing]

-- Foldable and Applicative = sequenceA_ & traverseA_ returns f () or f [..]
-- If you want to t (f a) -> f (t a) - use Traversable

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f ta = foldr (\ a b -> (:) <$> f a <*> b ) (pure []) ta


test3 = traverse2list (\x -> [x+10,x+20]) [1,2,3]

-- class (Functor t, Foldable t) => Traversable t where
--     sequenceA :: Applicative f => t (f a) -> f (t a)
--     traverse :: Applicative f => (a -> f b) -> t a -> f (t b)