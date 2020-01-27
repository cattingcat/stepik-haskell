{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ThinkingWithTypes.OpenProduct where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Fcf hiding (type (+), Any)


data Any f where 
  Any :: f t -> Any f
  
data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where 
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts


nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (name :: Symbol) = Key

insert :: (UniqueKey key ts ~ 'True) => Key key -> f t -> OpenProduct f ts -> OpenProduct f ( '(key, t) ': ts)
insert _ v (OpenProduct vect) = OpenProduct (V.cons (Any v) vect)

-- <=< - composition
type UniqueKey (key :: Symbol) (ts :: [(Symbol, k)]) = Eval (Null =<< Filter (TyEq key <=< Fst) ts)


tst1 = insert (Key @"kek") (Just "kek val") nil


type FindElemIndex (key :: Symbol) (ts :: [(Symbol, k)]) = Eval (FromMaybe Stuck (Eval (FindIndex (TyEq key <=< Fst) ts)))

type FindElemType (key :: Symbol) (ts :: [(Symbol, k)]) = Eval (FromMaybe Stuck =<< Lookup key ts)

findElem :: forall key ts . KnownNat (FindElemIndex key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElemIndex key ts)

get :: 
  forall key f t ts . KnownNat (FindElemIndex key ts) => 
  Key key -> OpenProduct f ts -> f (FindElemType key ts)
get _ (OpenProduct vect) = let aft = V.unsafeIndex vect (findElem @key @ts) in case aft of (Any ft) -> unsafeCoerce ft

tst2 = get (Key @"kek") tst1


type UpdateItem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) = Eval (SetIndex (FindElemIndex key ts) '(key, t) ts)


update :: 
  forall key f t ts . KnownNat (FindElemIndex key ts) => 
  Key key -> f t -> OpenProduct f ts -> OpenProduct f (UpdateItem key t ts)
update _ v (OpenProduct vect) = let any = Any v in 
  OpenProduct $ V.unsafeUpd vect [(findElem @key @ts, any)]


type Delete (key :: Symbol) (ts :: [(Symbol, k)]) = Eval (Filter (TyEq key <=< Fst) ts)

delete :: 
  forall key f ts . KnownNat (FindElemIndex key ts) => 
  Key key -> OpenProduct f ts -> OpenProduct f (Delete key ts) 
delete _ (OpenProduct vect) = OpenProduct $ V.drop (findElem @key @ts) vect




-- some mistake here
--type Contains (key :: Symbol) (ts :: [(Symbol, k)]) = Eval (Not (Eval (TyEq 'Nothing (UpsertMaybe 0 key ts))))


type family Contains (key :: Symbol) (ts :: [(Symbol, k)]) :: Bool where 
  Contains key '[] = 'False
  Contains key ('(key, v) ': t) = True
  Contains key ('(k, v) ': t) = Contains key t
  
type family UpsertMaybe (acc :: Nat) (key :: Symbol) (ts :: [(Symbol, k)]) :: Maybe Nat where 
  UpsertMaybe n key '[] = 'Nothing
  UpsertMaybe n key ('(key, v) ': t) = 'Just n
  UpsertMaybe n key ('(k, v) ': t) = UpsertMaybe (n+1) key t
  
class KnownMaybeNat (m :: Maybe Nat) where 
  maybeVal :: Maybe Int
  
instance KnownMaybeNat 'Nothing where 
  maybeVal = Nothing
  
instance (KnownNat n) => KnownMaybeNat ('Just n) where 
  maybeVal = Just $ fromIntegral . natVal $ Proxy @n
  
idxIfExist :: forall key ts . KnownMaybeNat (UpsertMaybe 0 key ts) =>  Maybe Int 
idxIfExist = maybeVal @(UpsertMaybe 0 key ts)
  

type Upsert (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) = If (Contains key ts) ts ( '(key, t) ': ts)

upsert :: 
  forall key f t ts . KnownMaybeNat (UpsertMaybe 0 key ts) => 
  Key key -> f t -> OpenProduct f ts -> OpenProduct f (Upsert key t ts)
upsert _ v (OpenProduct vect) = case idxIfExist @key @ts of 
  Nothing -> OpenProduct $ V.cons (Any v) vect
  Just i  -> OpenProduct $ V.unsafeUpd vect [(i, Any v)]
  
  
tst31 = insert (Key @"kek") (Just "kek val") nil
tst32 = upsert (Key @"puk") (Just "puk val") tst31
tst33 = upsert (Key @"kek") (Just "kek22 val") tst32
tst34 = get (Key @"kek") tst33



-- | Overloaded labels ext
--  #foo     ->      fromLabel @"foo"

-- ~ - constraint trick
instance (key ~ key') => IsLabel key (Key key') where 
  fromLabel = Key

tst41 = insert #kek (Just "kek val") nil
tst42 = upsert #puk (Just "puk val") tst41
tst43 = upsert #kek (Just "kek22 val") tst42
tst44 = get #kek tst33