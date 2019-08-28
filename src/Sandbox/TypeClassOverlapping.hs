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
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Sandbox.TypeClassOverlapping where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

data Format = JSON | CSV | PlainText | XML


class ToJSON a      where toJson :: a -> String
class ToCSV a       where toCsv :: a -> String
class ToPlainText a where toPlainText :: a -> String
class ToXML a       where toXml :: a -> String

class Serializable (p :: Format) a where serialize :: a -> String -- AllowAmbiguousTypes

instance (ToJSON a) => Serializable JSON a      where serialize = toJson
instance (ToCSV a) => Serializable CSV a        where serialize = toCsv
instance (ToPlainText a) => Serializable PlainText a where serialize = toPlainText
instance (ToXML a) => Serializable XML a        where serialize = toXml

class TrySerialize (formats :: [Format]) (f :: Format) a where trySer :: a -> Maybe String


instance {-# OVERLAPPABLE #-}                      TrySerialize fs        f a where trySer _ = Nothing
instance {-# OVERLAPPING #-} (Serializable f a) => TrySerialize (f ': fs) f a where trySer   = Just . serialize @f -- TypeApplication
instance {-# OVERLAPS #-} (TrySerialize fs f a) => TrySerialize (q ': fs) f a where trySer   = trySer @fs @f


data a :## (formats :: [Format])

--foo :: forall fs f a. TrySerialize fs f a => Proxy a :## fs -> Format -> a -> Maybe String
--foo _ JSON = trySer @fs @JSON
--foo _ CSV = trySer @fs @CSV
--foo _ PlainText = trySer @fs @PlainText
--foo _ XML = trySer @fs @XML
