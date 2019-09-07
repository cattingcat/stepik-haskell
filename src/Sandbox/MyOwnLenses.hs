{-# LANGUAGE RankNTypes, TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-} -- default in type classes
{-# LANGUAGE GADTs #-} -- ~ operator

-- https://artyom.me/lens-over-tea-1
module Sandbox.MyOwnLenses where

import Control.Applicative
import Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (sa, sb) = let fb = f sa in fmap (,sb) fb

tst11 = _1 (\a -> if a > 0 then Just a else Nothing) (1, 2)
tst12 = _1 (\a -> if a > 0 then Just a else Nothing) (-1, 2)
tst13 = _1 (\a -> (a, a)) (-1, 2)

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (sa, sb) = let sb' = f sb in fmap (sa,) sb'

-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = let i = f (get s) in fmap (set s) i

-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 _ f (Left s1) = let t1 = l1 f s1 in fmap Left t1
choosing _ l2 f (Right s2) = let t2 = l2 f s2 in fmap Right t2

-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (\a -> let b = f a in (b,b))

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (\a -> (a, f a))

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
united :: Lens' s ()
united f s = fmap (const s) (f ())


_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
  where
    update x
      | x < 0     = error "_abs: absolute value can't be negative"
      | otherwise = signum n * x

tst31 = runIdentity $ _abs (Identity . (^2)) (-5)

_all :: (Eq a) => a -> Lens' [a] a
_all a = lens get set where
  get s     = a
  set s new = fmap (\i -> if i == a then new else i) s

_all' :: (Eq a) => a -> Lens' [a] a
_all' a f s = let val = f a in fmap replace val where
  replace v = fmap (\i -> if i == a then v else i) s

tst40 = _all 5 (\i -> Just (-1)) []
tst41 = _all 5 (\i -> Just (-1)) [1,2,3,4,5,6,7,5,9]
tst42 = _all 5 (\i -> Just (-1)) [1,2,3,4,55,6,7,55,9]

tst40' = _all' 5 (\i -> Just (-1)) []
tst41' = _all' 5 (\i -> Just (-1)) [1,2,3,4,5,6,7,5,9]
tst42' = _all' 5 (\i -> Just (-1)) [1,2,3,4,55,6,7,55,9]

-- We have a lot of entries of 5, but "Kek" printed only once
--  it is because of Functor
tst51 = _all 5 foo [5,5,5,5,5,5] where
  foo a = do putStrLn "Kek"; return 111

-- So, lets move to applicative lenses


type AppLens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a

_appAll :: (Eq a) => a -> AppLens' [a] a
_appAll a f = traverse mf where
  mf a' = if a == a' then f a else pure a'

-- Not it works ok
tst61 = _appAll 5 foo [5,5,5,5,5,5] where
  foo a = do putStrLn "Kek"; return 111


-- Too strict Lens type, we can't pass AppLens, so, lets abstract from Lens
{-
  view :: Lens s t a b -> s -> a
  over :: Lens s t a b -> (a -> b) -> s -> t
  set  :: Lens s t a b -> b -> s -> t
-}

-- Specific lens types
type Getting s a = (a -> Const a a) -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t


view :: Getting s a -> s -> a
view l = getConst . l Const

-- We need monoid for instance Applicative (Const m)
instance Semigroup Int where
  (<>) = (+)
instance Monoid Int where
  mempty = 0

tst71 :: Int -- (Monoid a, Num a, Eq a, Show a) => a
tst71 = view (_appAll 5) [1,2,3,4,5,6,7,8,9,5]


toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf l = getConst . l (\a -> Const [a])

tst71' = toListOf (_appAll 5) [1,2,3,4,5,6,7,8,9,5]


over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

tst81 = over (_appAll 5) (const 99) [1,2,3,4,5,6,7,8,9,5]


set  :: Setting s t a b -> b -> s -> t
set l b s = runIdentity $ l (const $ Identity b) s

tst91 = set (_appAll 5) 99 [1,2,3,4,5,6,7,8,9,5]


newtype First a = First (Maybe a)
instance Semigroup (First a) where
  (<>) a@(First (Just _)) _ = a
  (<>) _ a = a

instance Monoid (First a) where
  mempty = First Nothing

preview :: ((a -> Const (First a) a) -> s -> Const (First a) s) -> s -> Maybe a
preview l s = let Const(First ma) = l foo s in ma where
  foo = Const . First . Just

tst101 = preview (_appAll 5) [1,2,3,4,5,6,7,8,9,5]


newtype Any = Any Bool

instance Semigroup Any where
 (<>) a@(Any True) _ = a
 (<>) _ a = a

instance Monoid Any where
 mempty = Any False

has :: ((a -> Const Any a) -> s -> Const Any s) -> s -> Bool
has l s = let (Const (Any b)) = l (const $ Const (Any True)) s in b

tst111 = has (_appAll 5) [1,2,3,4,5,6,7,8,9,5]


-- Types signatures too long, lets make it shorter

type Getting' r s a = (a -> Const r a) -> s -> Const r s
type Getting'' s a = Getting' a s a

has' :: Getting' Any s a -> s -> Bool
has' = has

preview' :: Getting' (First a) s a -> s -> Maybe a
preview' = preview


-- Real name in Lens library
type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b

  -- You have to use all type vars from class definition, so we use ~ type equivalence
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b 
  each = traverse

instance (Traversable t) => Each (t a) (t b) a b where
  each = traverse
  
  
-- The reason of instances like that:
--   instance (a ~ a', b ~ b') => Each (a, a') (b, b') a b
-- is Haskell's instance search algo. It searcher instance by signature (not by context)
-- so it will find this instance for tuples with different types
-- but will say about un-equivalence