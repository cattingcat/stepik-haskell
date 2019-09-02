{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeApplications      #-}



module Sandbox.TrickySerialization where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

data Format = JSON | CSV | PlainText | XML

class ToJSON a where
  toJson :: a -> String

class ToCSV a where
  toCsv :: a -> String

class ToPlainText a where
  toPlainText :: a -> String

class ToXML a where
  toXml :: a -> String


data (a :: *) :## (formats :: [Format])

type family FormatConstr (formats :: [Format]) (a :: *) :: Constraint where
  FormatConstr '[] _ = ()
  FormatConstr (JSON ': ts) a = (ToJSON a, FormatConstr ts a)
  FormatConstr (CSV ': ts) a = (ToCSV a, FormatConstr ts a)
  FormatConstr (PlainText ': ts) a = (ToPlainText a, FormatConstr ts a)
  FormatConstr (XML ': ts) a = (ToXML a, FormatConstr ts a)

--type family GatherCommConstr (formats :: [Format]) (a :: *) :: Constraint where
--  GatherCommConstr '[] _ = ()
--  GatherCommConstr (JSON ': ts)       a  = (CommonSerializable (Proxy 'JSON) a,      GatherCommConstr ts a)
--  GatherCommConstr (CSV ': ts)        a  = (CommonSerializable (Proxy 'CSV) a,       GatherCommConstr ts a)
--  GatherCommConstr (PlainText ': ts)  a  = (CommonSerializable (Proxy 'PlainText) a, GatherCommConstr ts a)
--  GatherCommConstr (XML ': ts)        a  = (CommonSerializable (Proxy 'XML) a,       GatherCommConstr ts a)

class Serializable (p :: Format) a where serialize :: a -> String

instance (ToJSON a) => Serializable JSON a            where serialize = toJson
instance (ToCSV a) => Serializable CSV a              where serialize = toCsv
instance (ToPlainText a) => Serializable PlainText a  where serialize = toPlainText
instance (ToXML a) => Serializable XML a              where serialize = toXml


class TrySerializable (fs :: [Format]) (f :: Format) a where trySerialize :: a -> Maybe String

instance {-# OVERLAPPING #-} (Serializable f a) => TrySerializable (f ': fs) f a where trySerialize    = Just . serialize @f -- TypeApplication
instance {-# OVERLAPS #-} (TrySerializable fs f a) => TrySerializable (q ': fs) f a where trySerialize = trySerialize @fs @f
instance {-# OVERLAPPABLE #-}                      TrySerializable fs        f a where trySerialize _  = Nothing



createSerializer :: forall fs f a. (FormatConstr fs a, TrySerializable fs f a) => Proxy (a :## fs) -> Format -> a -> Maybe String
createSerializer = undefined
--createSerializer _ JSON       = trySerialize @fs @JSON @a
--createSerializer _ CSV        = trySerialize @fs @CSV @a
--createSerializer _ PlainText  = trySerialize @fs @PlainText @a
--createSerializer _ XML        = trySerialize @fs @XML @a



-- -- Client code -- --

type SSTupleSerializer = (String, String) :## [JSON, CSV]

instance ToJSON (String, String) where
  toJson (s1, s2) = "{ _1: " ++ s1 ++ ", _2: " ++ s2 ++ "}"

instance ToCSV (String, String) where
  toCsv (s1, s2) = s1 ++ ", " ++ s2

ssTupleSerializer :: Format -> (String, String) -> Maybe String
ssTupleSerializer = _ -- createSerializer (Proxy :: Proxy SSTupleSerializer)

tst1 = ssTupleSerializer JSON ("kek", "puk")      -- Just
tst2 = ssTupleSerializer CSV ("kek", "puk")       -- Just
tst3 = ssTupleSerializer PlainText ("kek", "puk") -- Nothing
