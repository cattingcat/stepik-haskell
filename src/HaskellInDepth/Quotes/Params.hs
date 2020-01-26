module HaskellInDepth.Quotes.Params where

import Options.Applicative


data Params = Params {
  fileName :: FilePath,
  company :: String,
  prices :: Bool,
  volumes :: Bool,
  html :: Bool,
  noText :: Bool
}

mkParams :: Parser Params
mkParams =
  Params <$> strArgument (metavar "FILE" <> help "CSV file")
         <*> strOption (long "company" <> short 'c' <> help "company name" <> value "")
         <*> switch (long "prices" <> short 'p' <> help "create prices chart")
         <*> switch (long "volumes" <> short 'v' <> help "create volumes chart")
         <*> switch (long "html" <> help "Create html report")
         <*> switch (long "no-text" <> short 'n' <> help "don't print statistics report")


cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper) (fullDesc <> progDesc "Stock quotes and dapa processing")

