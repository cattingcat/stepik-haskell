{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE MultiParamTypeClasses #-} -- only mutliparam classes
{-# LANGUAGE FunctionalDependencies #-} 
{-# LANGUAGE FlexibleInstances #-} 

module HsFp2.MonadsAndEffects.ImplicitLift.Mtl where

infixl 7 ***

class Mult a b c | a b -> c where 
    (***) :: a -> b -> c

instance Mult Int Int Int where 
    (***) = (*)

instance Mult Int Double Double where 
    a *** b = fromIntegral a * b

instance Mult Double Int Double where 
    a *** b = a * fromIntegral b

-- You need to specify all types because of type deduction for instance getting
-- so we use " | a b -> c " in class declaration
tst11 = ((5 :: Int) *** (5.5 :: Double)) :: Double

tst12 = (5.5 :: Double) *** (5 :: Int)



class Functor' c e | c -> e where
    fmap' :: (e -> e) -> c -> c

instance Functor m => Functor' (m a) a where 
    fmap' = fmap 

-- instance Functor' (Maybe n) n where 
--     fmap' f (Nothing) = Nothing
--     fmap' f (Just a) = Just (f a)

-- instance Functor' ([a]) a where 
--     fmap' _ [] = []
--     fmap' f (a:as) = (f a) : fmap' f as
    

tst21 = fmap' succ "ABC" -- "BCD"
tst22 = fmap' (^2) (Just 42) -- Just 1764