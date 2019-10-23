{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module Sandbox.StanislavExample where

import Control.Arrow
import Control.Category
import Prelude hiding ((.))

d :: [(a, a')]
d = []


f1 :: a -> Maybe b
f1 = undefined

f2 :: b -> Maybe c
f2 = undefined

f3 :: c -> Maybe d
f3 = undefined

fn :: d -> Maybe z
fn = undefined

comp' = foldr (\a r -> Kleisli a . r) (Kleisli pure) [f1, f2, f3, fn]

--comp'' = f1 >>> f2 >>> f3 >>> fn

comp = Kleisli f1 . Kleisli f2 . Kleisli f3 . Kleisli fn

result :: [(a, a')] -> Maybe [(z, a')]
result = mapM ((runKleisli . first) comp)



foo :: Monad m => Kleisli m a b -> (a, z) -> m (b, z)
foo (Kleisli f) (a, z) = (, z) <$> f a

-- see Arrow type-class
bar :: Monad m => Kleisli m a b -> Kleisli m (a, z) (b , z)
bar f = first f



