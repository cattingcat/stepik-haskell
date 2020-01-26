{-# LANGUAGE DeriveAnyClass #-}

module HaskellInDepth.Quotes.Statistics (
  Statistic(..),
  StatEntry(..),
  StatQFieldData,
  StatInfo,

  statInfo
) where

import HaskellInDepth.Quotes.BoundedEnum
import HaskellInDepth.Quotes.QuoteData
import Data.Time (diffDays)
import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)


data Statistic = Mean | Min | Max | Days
  deriving (Show, Eq, Enum, Bounded, BoundedEnum)

data StatEntry = StatEntry {
  stat :: Statistic,
  qfield :: QField,
  value :: Fixed4
}

type StatQFieldData = (QField, [StatEntry])
type StatInfo = [StatQFieldData]

--mean :: (Foldable t, Num a, Fractional a) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

--daysBetween :: (Foldable t, Num n) => QField -> t QuoteData -> n
daysBetween qf quotes = fromIntegral $ abs $ diffDays dMinQuote dMaxQuote
  where
    cmp = comparing (field2fun qf)
    dMinQuote = day $ minimumBy cmp quotes
    dMaxQuote = day $ maximumBy cmp quotes

--funcByField :: (Foldable t, Functor t) => (t Fixed4 -> a) ->  QField -> (t QuoteData -> a)
funcByField func qf = func . fmap (field2fun qf)

--computeStats :: (Foldable t, Functor t) => Statistic -> (QField -> t QuoteData -> Fixed4)
computeStats Mean = funcByField mean
computeStats Min = funcByField minimum
computeStats Max = funcByField maximum
computeStats Days = daysBetween

statInfo :: (Functor t, Foldable t) => t QuoteData -> StatInfo
statInfo quotes = map stQFData range
  where
    stQFData qf = (qf, [StatEntry st qf v | st <- range, let v = computeStats st qf quotes])