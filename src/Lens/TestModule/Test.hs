{-# LANGUAGE DuplicateRecordFields #-}

module Lens.TestModule.Test where

import Prelude hiding (id)

import Lens.TestModule.Kek
import Lens.TestModule.Puk
import Control.Lens


kek = MkKek 1 "kek"
puk = MkPuk 1 "kek"

--tst11 = (kek ^. dt) ++ (puk ^. dt) 