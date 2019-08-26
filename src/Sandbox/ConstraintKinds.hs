{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}


module Sandbox.ConstraintKinds where 

import GHC.Exts (Constraint)


type family Constr a :: Constraint where 
    Constr [a] = (Eq a)
    Constr (Maybe a) = ()


class MyClass a where 
    foo :: Constr a => a -> Int

-- UndecidableSuperClasses
class (Constr a) => MyClass2 a where 
    bar :: a -> Int

newtype MyNum = MyNum Int

instance MyClass ([MyNum]) where 
    foo a = 55

-- Cant do that because no Eq instance for MyNum
-- instance MyClass2 ([MyNum]) where 
--     bar a = 55

-- Cant do that too
-- tst1 = foo ([MyNum 55])