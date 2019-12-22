{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lens.TestModule.Puk where

import Prelude hiding (id)
import Control.Lens
import Control.Lens.TH


data Puk = MkPuk { _fooId :: Int, _fooDt :: String }

makeFields ''Puk