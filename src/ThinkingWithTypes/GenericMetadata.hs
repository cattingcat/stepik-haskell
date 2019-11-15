{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ThinkingWithTypes.GenericMetadata where

import Control.Monad.Writer
import Data.Aeson (Value (..), (.=), object, encode)
import Data.Kind (Type)
import Data.Text (Text, pack)
import Data.Typeable
import Data.Vector (fromList)
import GHC.Generics
import GHC.TypeLits
import qualified GHC.TypeLits as Err

data Person = Person {
  name :: String,
  age :: {-# UNPACK #-} !Int,
  phone :: Maybe String,
  permissions :: [Bool]
} deriving (Generic)

-- | :kind! Rep Person
-- Any modifiers affect metadata :
-- !String - DecidedStrict
-- {-# UNPACK #-} - SourceUnpack (it also requires !)

class GSchema (a :: Type -> Type) where
  gschema :: Writer [Text] Value

mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object $ a <> b

emitRequired :: forall nm . KnownSymbol nm => Writer [Text] ()
emitRequired = tell . lst . pack . symbolVal $ Proxy @nm

lst :: a -> [a]
lst a = [a]

type family RepName (rep :: Type -> Type) :: Symbol where
  RepName (D1 (MetaData nm _ _ _) _) = nm

type family TypeName (t :: Type) :: Symbol where
  TypeName t = RepName (Rep t)

type family ToJSONType (a :: Type) :: Symbol where
  ToJSONType Int = "integer"
  ToJSONType Integer = "integer"
  ToJSONType Float = "number"
  ToJSONType Double = "number"
  ToJSONType String = "string"
  ToJSONType Bool = "boolean"
  ToJSONType [a] = "array"
  ToJSONType a = TypeName a

makeTypeObj :: forall o . KnownSymbol (ToJSONType o) => Value
makeTypeObj = let
  val = symbolVal $ Proxy @(ToJSONType o)
  jsonStr = String $ pack val
 in object ["type" .= jsonStr]

tst1 = makeTypeObj @Int

makePropertyObj :: forall nm . KnownSymbol nm => Value -> Value
makePropertyObj val = let
  name = pack . symbolVal $ Proxy @nm in object [name .= val]

-- 'MetaSel - descriptor of properties
-- S1 = M1 S   - metadata for record selector
instance (KnownSymbol nm, KnownSymbol (ToJSONType propType)) =>
  GSchema (S1 ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 propType)) where
  gschema = do
    emitRequired @nm
    pure . makePropertyObj @nm $ makeTypeObj @propType

-- Special processing of Maybe values
instance {-# OVERLAPPING #-} (KnownSymbol nm, KnownSymbol (ToJSONType propType)) =>
  GSchema (S1 ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 (Maybe propType))) where
  gschema = pure . makePropertyObj @nm $ makeTypeObj @propType

-- Special processing of list values
instance {-# OVERLAPPING #-} (KnownSymbol nm, KnownSymbol (ToJSONType [propType]), KnownSymbol (ToJSONType propType)) =>
  GSchema (S1 ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 [propType])) where
  gschema = do
    emitRequired @nm
    let propObjBuilder = makePropertyObj @nm
    let typeObj = makeTypeObj @[propType]
    let innerArrType = ["items" .= makeTypeObj @propType]
    let typeObjComb = mergeObjects typeObj $ object innerArrType
    pure . propObjBuilder $ typeObjComb

-- Special processing for strings
instance {-# OVERLAPPING #-} KnownSymbol nm =>
  GSchema (M1 S ('MetaSel ('Just nm) _1 _2 _3) (K1 _4 String)) where
  gschema = do
    emitRequired @nm
    pure . makePropertyObj @nm $ makeTypeObj @String

instance (GSchema a, GSchema b) => GSchema (a :*: b) where
  gschema = do
    va <- gschema @a
    vb <- gschema @b
    let s = mergeObjects va vb
    pure s

instance (TypeError ('Err.Text "JSON Schema does not support sum types")) =>
  GSchema (a :+: b) where
  gschema = error ""

-- C1 = M1 C   - metadata for ctors
instance GSchema a => GSchema (M1 C _1 a) where
  gschema = gschema @a

-- D1 = M1 D   - metadata of whole data type
instance (GSchema a, KnownSymbol nm) => GSchema (M1 D (MetaData nm _1 _2 _3) a) where
  gschema = do
    innerSchema <- gschema @a
    pure $ object
      [
        "title" .= (String . pack . symbolVal $ Proxy @nm),
        "type" .= String "object",
        "properties" .= innerSchema
      ]

schema ::forall a . GSchema (Rep a) => Value
schema = let (obj, requires) = runWriter $ gschema @(Rep a)
  in mergeObjects obj $ object [ "required" .= Array (fromList $ String <$> requires)]

tst2 = encode $ schema @Person