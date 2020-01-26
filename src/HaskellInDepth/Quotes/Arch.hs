module HaskellInDepth.Quotes.Arch where

import qualified Data.Text as T

data Params
data QuoteData
data QuoteDataCollection
data StatsInfo


work :: Params -> IO ()
work = undefined

readQuotes :: FilePath -> QuoteDataCollection
readQuotes = undefined

statsInfo :: QuoteDataCollection -> StatsInfo
statsInfo = undefined

statsReport :: StatsInfo -> T.Text
statsReport = undefined

plotCharts :: Params -> QuoteDataCollection -> IO ()
plotCharts = undefined