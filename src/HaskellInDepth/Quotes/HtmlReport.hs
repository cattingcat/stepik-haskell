{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HaskellInDepth.Quotes.HtmlReport (
  htmlReport
) where

import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Fmt

import HaskellInDepth.Quotes.QuoteData
import HaskellInDepth.Quotes.StatReport (showStatEntryValue)
import HaskellInDepth.Quotes.Statistics



htmlReport :: (Functor t, Foldable t) =>
 String -> t QuoteData -> StatInfo -> [FilePath] -> ByteString
htmlReport docTitle dataCollection stats chartImages = renderHtml $ docTypeHtml $ do
  H.head $ do
    H.title $ string docTitle
    H.style style
  body $ do
    renderDiagrams chartImages
    renderStatInfo stats
    renderData dataCollection

  where
    style = "table {border-collapse: collapse} td, th {border: 1px solid black; padding: 3px}"
    
    renderDiagrams [] = pure ()
    renderDiagrams images = do 
      h1 "Diagrams"
      traverse_ ((img!).src.toValue) images
      
    renderStatInfo [] = pure ()
    renderStatInfo si@((_, ses):_) = do
      h1 "Stats report"
      table $ do
        thead $ tr $ traverse_ th $
          "Quotes field" : [text $ fmt $ build $ stat s | s <- ses]
        tbody $ traverse_ statData2Tr si
        
    renderData quotes = do 
      h1 "Stock Quotes data"
      table $ do
        thead $ tr $ traverse_ th ["Day", "Close", "Volume", "Open", "High", "Low"]
        tbody $ traverse_ quoteData2Tr quotes
     
    quoteData2Tr QuoteData{..} = tr $ do
      td $ string $ show day
      traverse_ (td . string . show) [close, volume, open, high, low]
    
    statData2Tr (qf, entries) = tr $ do 
      td $ string $ show qf
      traverse_ (td . string . showStatEntryValue) entries