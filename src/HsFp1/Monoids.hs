module HsFp1.Monoids where
    
import Prelude(Maybe(..), Show(..), Eq(..), ($))

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a 


newtype Maybe' a = Maybe' { getMaybe :: Maybe a } deriving (Eq, Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' $ Just mempty
    mappend (Maybe' a) (Maybe' b) = case (a, b) of
        (_, Nothing)        -> Maybe' Nothing
        (Nothing, _)        -> Maybe' Nothing
        (Just a, Just b)    -> Maybe' $ Just (mappend a b)