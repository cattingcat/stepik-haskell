module HsFp1.MapLike where 

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq, Show)

instance MapLike ListMap where 
    empty = ListMap []
    lookup k = takeFirst . (L.filter (\(k',_) -> k' == k)) . getListMap where 
        takeFirst []    = Nothing
        takeFirst ((_, v):_) = Just v
    insert k v m = ListMap $ (k,v) : (filter (\(k', v') -> k' /= k) (getListMap m))
    delete k m = ListMap $ filter (\(k', v') -> k' /= k) (getListMap m)


test = ListMap [(1,1), (2,2), (3,3)]


newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }


instance MapLike ArrowMap where 
    empty = ArrowMap $ \_ -> Nothing
    lookup k (ArrowMap m) = m k
    insert k v (ArrowMap m) = ArrowMap $ \k' -> if k == k' then Just v else m k
    delete k (ArrowMap m) = ArrowMap $ \k' -> if k == k' then Nothing else m k
    fromList [] = empty
    fromList l = ArrowMap $ \k -> lookup k l where
        lookup _ [] = Nothing
        lookup k ((k',v'):as) = if k == k' then Just v' else lookup k as