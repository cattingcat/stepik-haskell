{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}

module Sandbox.MoreFunnyExts where
    
import GHC.Exts (Constraint)
-- import Data.Hashable
-- import Data.HashSet





data Person = Person { id :: Int }
data Animal = Animal { id :: Int }

-- test :: (Int, Int, Int)
-- test = (id (Person 1), id (Animal 2), id (Animal 3))

class MyClass a where 
    kek :: a -> String

-- instance MyClass (Maybe a) where 
--     kek :: Maybe a -> String
--     kek (Just a) = "Just"
--     kek _        = "Nothing"

-- FlexibleInstances - instance for more specific types
instance MyClass (Maybe Int) where 
    kek :: Maybe Int -> String
    kek (Just a) = "Just Int"
    kek _        = "Nothing Int"




-- instance (MyClass a) => MyClass (Either a b) where 
--     kek _ = "Either"

-- FlexibleContexts - context with specific type
instance (MyClass (Maybe a)) => MyClass (Either a b) where 
    kek _ = "Either From Maybe"


-- tst1 :: String
-- tst1 = kek $ (Just "")

-- tst2 :: String
-- tst2 = kek $ ((Just 55) :: Maybe Int)


data Data1 a = Data1 a
data Data2 a = Data2 a

-- ~ - type equality, GADT ext
foo :: (a ~ b) => Data1 a -> Data2 b -> a
foo (Data1 a) (Data2 b) = b


-- TypeFamilies - function on types
type family TypeFoo a 
type instance TypeFoo Int = String

fooTypeFoo :: a -> TypeFoo a
fooTypeFoo = undefined

class Convertible a where 
    type Conv a
    convert :: a -> Conv a



-- type family with constraint
type family Con a :: Constraint
type instance Con [a] = (Ord a)
type instance Con (Maybe a) = (Eq a)

class Sized a where 
    famConFoo :: Con a => a -> Int
    
instance Sized [a] where
    famConFoo = length

instance Sized (Either b a) where 
    famConFoo = undefined

-- instance Sized (Maybe a) where 
--     -- famConFoo :: Maybe a -> Int
--     famConFoo = 2