{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellInDepth.Quotes.QuoteData where

import HaskellInDepth.Quotes.BoundedEnum

import Data.Fixed
import Data.Time
import Data.Csv
import GHC.Generics (Generic)
import Safe (readDef)
import Data.ByteString
import Data.ByteString.Internal (w2c)

data E4
instance HasResolution E4 where
  resolution _ = 10000

type Fixed4 = Fixed E4

instance FromField Fixed4 where 
  parseField = pure . readDef (0 :: Fixed4) . fmap w2c . unpack 
  
instance FromField Day where 
  parseField s = parseTimeM False defaultTimeLocale "%Y/%m/%d" (w2c <$> unpack s)

data QuoteData = QuoteData {
  day :: Day,
  close :: Fixed4,
  volume :: Fixed4,
  open :: Fixed4,
  high :: Fixed4,
  low :: Fixed4
} deriving (Generic, FromNamedRecord)


data QField = Open | Close | High | Low | Volume
  deriving (Bounded, Enum, BoundedEnum)

field2fun :: QField -> QuoteData -> Fixed4
field2fun Open = open
field2fun Close = close
field2fun High = high
field2fun Low = low
field2fun Volume = volume