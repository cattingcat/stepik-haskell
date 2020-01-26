{-# LANGUAGE StandaloneDeriving #-}
module HaskellInDepth.Quotes.Charts (
  plotChart
) where

import Data.Foldable
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

import HaskellInDepth.Quotes.QuoteData

deriving instance Show QField

plotChart :: (Functor t, Foldable t) => String -> t QuoteData -> [QField] -> FilePath -> IO ()
plotChart title dataCollection fields outFileName = toFile fileOpts outFileName $ do
  layout_title .= title
  traverse_ plotLine fields
  where 
    fileOpts = FileOptions (800, 600) SVG loadSansSerifFonts
    plotLine qf = plot $ line (show qf) [toList $ fmap (qf2pd qf) dataCollection]
    qf2pd qf q = (day q, realToFrac $ field2fun qf q :: Double)