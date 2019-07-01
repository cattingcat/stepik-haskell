{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}

module HsFp2.TraversableComp where
import Data.Monoid
import Data.Maybe
import Data.Functor.Compose

infixr 9 |.|

newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Functor f, Functor g) => Functor (f |.| g) where
    fmap f (Cmps fga) = Cmps $ fmap (fmap f) fga

instance (Applicative f, Applicative g) => Applicative (f |.| g) where
    pure = Cmps . pure . pure
    (<*>) (Cmps f) (Cmps a) = Cmps $ (<*>) <$> f <*> a

instance (Foldable f, Foldable g) => Foldable (f |.| g) where
    foldMap f (Cmps fga) = foldMap (foldMap f) fga

instance (Traversable f, Traversable g) => Traversable (f |.| g) where
    -- :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
    -- (a -> f b) -> fg a -> f (fg b)
    traverse f (Cmps fga) = fmap Cmps $ traverse (\ga -> traverse f ga) fga


-- ([] |.| Maybe) (Either Int) -> Either (([] |.| Maybe) Int)
test1 = sequenceA (Cmps [Just (Right 2), Nothing]) -- Right (Cmps {getCmps = [Just 2,Nothing]})
test2 = sequenceA (Cmps [Just (Left 2), Nothing])  --Left 2



-- data Nat = Z | S Nat deriving (Show, Eq)
-- data Nat where
--     Z :: Nat
--     S :: Nat -> Nat     
--     deriving (Show, Eq)

-- five = S (S (S (S (S Z))))

-- Nice example of Kins usage!
newtype Compose' (f :: k -> *) (g :: k1 -> k) (a :: k1) = Compose' (f (g a))

compTest = \x -> fmap Right (Just x)

compTest2 :: b -> Compose' (Either a) Maybe b
compTest2 x = Compose' (pure (pure x))

{--
sequenceA . fmap Compose == Compose . fmap sequenceA . sequenceA
(Applicative f, Applicative g, Traversable t) => t (f (g a)) -> Compose f g (t a)

sequenceA :: Applicative f => t (f a) -> f (t a)
Compose :: f (g a) -> Compose...
fmap :: f a -> f b

sequenceA :: t (f (g a))  -> f (t (g a))
fmap sequenceA  :: f (t (g a)) -> f (g (t a))


--}