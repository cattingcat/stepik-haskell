{-# LANGUAGE TemplateHaskell #-}

module Sandbox.LensesFun where

import Control.Lens
import Control.Lens.TH
import Data.Map

-- Write Lenses manually

data Person = Person {
  _pName :: String,
  _pAge :: Int
} deriving (Show)

pName :: Lens' Person String
pName = lens _pName (\p n -> p { _pName = n })

ben = Person "Ben" 32

tst11 = ben ^. pName
tst12 = ben & pName .~ "Ben Kolera"



-- Write Lenses via template haskell

data Organization = Organization {
  _oName :: String,
  _oNo :: Int
} deriving (Show)

makeLenses ''Organization

ya = Organization "Ya" 12
tst21 = ya
  & oName .~ "Goo"
  & oNo .~ 13
  
  
-- Lens combination

data Company = Company {
  _cOwner :: Person,
  _cCto :: Person,
  _cOrgInfo :: Organization
} deriving (Show)

makeLenses ''Company

john = Person "John" 18

yaComp = Company ben john ya

tst31 = yaComp ^. (cOwner . pName) 
tst32 = yaComp & (cOwner . pName) %~ ("Kek " ++) 

getCompanyUrl :: Getting String Int String
getCompanyUrl = to (\id -> "https://all-comps.com/" ++ show id)

tst41 = yaComp ^. (cOrgInfo . oNo . getCompanyUrl)

testMap :: Map Int String
testMap = fromList [(1, "World")]

tst51 = testMap ^. at 1 -- %~ ("Hello" ++) 
--tst52 = testMap & at 1 %~ ("Hello" ++) 

