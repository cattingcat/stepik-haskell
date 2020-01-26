{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellInDepth.Quotes.StatReport (
  statReport,
  showStatEntryValue
) where

import Data.Fixed (showFixed)
import Data.Text (Text)
import Fmt
import HaskellInDepth.Quotes.QuoteData
import HaskellInDepth.Quotes.Statistics


instance Buildable Statistic where
  build Mean = "Mean"
  build Min = "Minimum"
  build Max = "Maximum"
  build Days = "Days between Min/Max"

showStatEntryValue :: StatEntry -> String
showStatEntryValue StatEntry{..} = showFixed (removeTrailing stat qfield) value
  where
    removeTrailing Days _ = True
    removeTrailing Min Volume = True
    removeTrailing Max Volume = True
    removeTrailing _ _ = False


instance Buildable StatEntry where
  build se@StatEntry{..} = ""+|stat|+": "+|showStatEntryValue se|+""

deriving instance Show QField
   

instance Buildable StatQFieldData where
  build (qfield, entries) = nameF ("Stats for " +||qfield||+ "") (unlinesF entries)
  
statReport :: StatInfo -> Text
statReport = fmt . unlinesF 