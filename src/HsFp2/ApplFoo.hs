module HsFp2.ApplFoo where

-- type fynction = a -> b
-- Applicative ((->) e)
-- (a -> b -> c) -> f a -> f b -> f c
-- (a -> b -> c) -> (e -> a) -> (e -> b) -> (e -> c)
-- combine          f           g
-- \e -> combine (f e) (g e)  :: e -> c

-- f (a -> b) -> f a -> f b