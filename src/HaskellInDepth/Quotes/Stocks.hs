{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellInDepth.Quotes.Stocks where
import HaskellInDepth.Quotes.QuoteData

import Data.Fixed
import Data.Time
import qualified Data.Text as T
import Fmt

tst1 :: Fixed4
tst1 = 3.141592234234234

fmtTst :: String
fmtTst =
  let
    name = "John" :: T.Text
    age = 20 :: Int
  in
    fmt $ "Hello, "+|name|+"! Your age is "+|age|+"."




isRising :: QuoteData -> Bool
isRising QuoteData{..} = close > open


text2quote :: T.Text -> [QuoteData]
text2quote = fmap (mkQuote . toComponents) . tail . T.lines
  where
    toComponents = fmap T.unpack . T.splitOn ","
    mkQuote (dt : rest@[_, _, _, _, _]) =
      let
        day = parseTimeOrError False defaultTimeLocale "%Y/%m/%d" dt
        [close, volume, open, high, low] = fmap read rest
      in QuoteData{..}
    mkQuote _ = error "Incorrect format"