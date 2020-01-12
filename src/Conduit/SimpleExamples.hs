{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Conduit.SimpleExamples where

import Data.Conduit
import qualified Data.Conduit.List as L
import Control.Monad.Identity (Identity)
import Control.Monad.Trans.Writer
import Control.Monad.Trans (MonadIO, liftIO, lift)


tst1 :: Identity Int
tst1 = L.sourceList [1 .. 100] $$ L.fold (+) 0

tst2 :: Identity Int
tst2 = runConduit $ L.sourceList [1 .. 100] .| L.filter odd .| L.map (+5) .| L.fold (+) 0

tst3 :: Identity [Int]
tst3 = runConduit $ L.sourceList [1, 2, 3] .| L.take 5


class Monad m => MonadKek m where
  kek :: m String
  puk :: String -> m ()

instance Monad m => MonadKek (WriterT String m) where
  kek = do
    tell "Kek"
    tell "puk"
    pure "Kek written"
  puk = tell
  
instance MonadKek Identity where 
  kek = pure "qwe"
  puk _ = pure ()
  
tst4Monad :: (MonadIO m, MonadKek m) => m String
tst4Monad = do
  liftIO $ print "lrl" 
  kek
  
tst4 = runWriterT (tst4Monad @(WriterT String IO))


tst5 :: MonadKek m => ConduitT Int Int m ()
tst5 = do
  i <- await 
  case i of 
    Just i -> if i > 0 
              then do 
                r <- lift kek
                yield 5
              else do 
                lift $ puk "qwe"
                yield 6
    Nothing -> pure ()
    
tst5Run :: WriterT String IO [Int]
tst5Run = runConduit $ L.sourceList [1 .. 100] .| tst5 .| L.take 5

tst5RunWr = runWriterT tst5Run