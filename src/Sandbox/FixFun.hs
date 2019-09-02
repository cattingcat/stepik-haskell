module Sandbox.FixFun where
    
import Control.Monad.Fix

testList :: [Int]
testList = [1..99]

-- fix :: (a -> a) -> a
-- fin f = f(f(f(f(f(f(f(...)))))))

-- fix :: ((a -> b) -> a -> b) -> a -> b

sumList :: [Int] -> Int
sumList is = fix foo is where
    foo f (a : []) = a
    foo f (a : as) = a + f as

type Hungry a = Int -> a

-- hungry :: Hungry a
-- hungry = fix foo where 
--     foo f n = f






-- -- -- Free Monad -- -- --

data Fix f = Fix (f (Fix f))

data Foo a = Foo a | Bar a

-- We have to change type
test0 :: Foo (Foo (Foo (Foo Int)))
test0 = Bar (Foo (Bar (Bar 5)))

-- We don't have to change type, but chain is infinite,
--  if the type doesn't contains termination
test1 :: Fix Foo
test1 = Fix (Foo (Fix (Bar test1)))

test2 :: Fix Foo
test2 = Fix (Foo (Fix (Foo test1)))

data FooTerm a = Foot a | End

test3 :: Fix FooTerm
test3 = Fix (Foot (Fix End))


-- Fix with terminating
data FixE f e = FixE (f (FixE f e)) | Throw e

--catch :: FixE f e -> (e -> FixE f e2) -> FixE f e2
--catch (FixE f) ff = undefined   -- We need to process fix inside 'f'
--catch (Throw e) ff = ff e

catch :: (Functor f) => FixE f e -> (e -> FixE f e2) -> FixE f e2
catch (FixE f) ff = FixE $ fmap (`catch` ff) f
catch (Throw e) ff = ff e


-- We've invented Free monad

data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Functor (Free f) where
  fmap f (Pure r) = Pure $ f r
  fmap f (Free fa) = Free $ fmap (fmap f) fa

instance (Functor f) => Applicative (Free f) where
  pure = Pure
--  (<*>) mf ma = mb
  (<*>) (Pure f) (Pure a) = Pure $ f a
  (<*>) f (Free fa) = Free $ fmap (f <*>) fa
  (<*>) (Free f) a = Free $ fmap (<*> a) f


instance (Functor f) => Monad (Free f) where
  return = Pure
  (>>=) (Pure r) f = f r
  (>>=) (Free fr) f = Free $ fmap (>>= f) fr

liftF :: (Functor f) => f a -> Free f a
liftF fa = Free $ fmap Pure fa


-- example --
data Toy a next = Say a next | Ring next | Done

instance Functor (Toy a) where
  fmap f (Say a n)  = Say a (f n)
  fmap f (Ring n)   = Ring (f n)
  fmap f Done       = Done

say :: a -> Free (Toy a) ()
say a = Free (Say a (Pure ()))

ring :: Free (Toy a) ()
ring = Free (Ring (Pure ()))

done :: Free (Toy a) ()
done = Free Done

say' a = liftF (Say a ())
ring' = liftF (Ring ())
done' = liftF Done

test4 :: Free (Toy String) ()
test4 = do
  say "Kek"
  ring
  done

interpretToy :: (Show a, Show r) => Free (Toy a) r -> String
interpretToy (Free (Say a cont)) = "Say " ++ show a ++ "; " ++ interpretToy cont
interpretToy (Free (Ring cont)) = "Ring; " ++ interpretToy cont
interpretToy (Free Done) = "Done;"
interpretToy (Pure r) = show r

interpretToyIo :: (Show a, Show r) => Free (Toy a) r -> IO ()
interpretToyIo (Free (Say a cont)) = do putStrLn ("Say " ++ show a ++ "; "); interpretToyIo cont
interpretToyIo (Free (Ring cont)) = do putStrLn "Ring; "; interpretToyIo cont
interpretToyIo (Free Done) = putStrLn "Done;"
interpretToyIo (Pure r) = print r
