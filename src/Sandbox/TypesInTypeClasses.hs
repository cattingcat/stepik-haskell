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

module Sandbox.TypesInTypeClasses where

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

class SomeClass a where 
  type IntType a :: *
  scfoo :: Proxy a -> String
  
instance SomeClass '[(a :: *), (b :: *)] where 
  type IntType '[a, b] = a -> b
  scfoo _ = "function"

foo :: (SomeClass a) => Proxy a -> IntType a -> String
foo p _ = scfoo p

testType :: Proxy '[Int, Int]
testType = Proxy

tst1 :: String
tst1 = foo testType (+ 11)





data Format = JSON | PlainText

class ToJson a where 
  toJson :: a -> String
  toJson _ = ""
  
class ToPlainText a where 
  toPlainText :: a -> String
  toPlainText _ = ""
  
  
instance ToJson String
instance ToPlainText String


data (sources :: [*]) :>> (r :: *) 
data f :## (formats :: [Format]) 

type family FormatConstraint (a :: [Format]) (t :: *) :: Constraint where
   FormatConstraint '[] _ = ()
   FormatConstraint (JSON ': ts) t = (ToJson t, FormatConstraint ts t)
   FormatConstraint (PlainText ': ts) t = (ToPlainText t, FormatConstraint ts t)
   
type family BuildFunc (sources :: [*]) (r :: *) :: * where 
  BuildFunc '[] r = r
  BuildFunc (t:ts) r = t -> BuildFunc ts r
  
type MyTestFunc = [Int, String] :>> String :## '[JSON]


type Tst1 = FormatConstraint '[JSON] String => BuildFunc [Int, Int] String
type Tst2 (sources :: [*]) (result :: *) (serializers :: [Format]) = FormatConstraint serializers result => BuildFunc sources result
type Tst3 = Tst2 [Int, String] String '[JSON]

-- :kind! Tst3


class HasConversion a where 
  type ConverterT a :: *
   
class CommonSerializable a p where 
  serialize :: p -> a -> String
  
instance ToJson a => CommonSerializable a (Proxy JSON) where serialize _ = toJson
instance ToPlainText a => CommonSerializable a (Proxy PlainText) where serialize _ = toPlainText
  
serialize' :: (FormatConstraint '[f] a, CommonSerializable a (Proxy f)) => Proxy f -> a -> String
serialize' = serialize  
   

instance FormatConstraint serializers result => HasConversion ((sources :: [*]) :>> (result :: *) :## (serializers :: [Format])) where 
  type ConverterT ((sources :: [*]) :>> (result :: *) :## (serializers :: [Format])) = BuildFunc sources result
  
createConverter :: HasConversion api => Proxy api -> ConverterT api -> String
createConverter p f = "kek"



type TstApiType = [Int, Int, String] :>> String :## '[JSON]

tstConverter = createConverter (Proxy :: Proxy TstApiType) foo where 
  foo i1 i2 s = s