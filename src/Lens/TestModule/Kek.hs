{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lens.TestModule.Kek where

import Prelude hiding (id)
import Control.Lens
import Control.Lens.TH

data Kek = MkKek { _fooId :: Int, _fooDt :: String }

makeFields ''Kek