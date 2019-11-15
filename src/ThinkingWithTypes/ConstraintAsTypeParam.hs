{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ThinkingWithTypes.ConstraintAsTypeParam where

import GHC.Base (Constraint)

data Dict (a :: Constraint) where
  Dict :: a => Dict a

data SignBool (b :: Bool) where
  STrue  :: SignBool 'True
  SFalse :: SignBool 'False

data SomeSBool where
  SomeSBool :: SignBool b -> SomeSBool

withSomeSBool :: SomeSBool -> (forall b . SignBool b -> r) -> r
withSomeSBool (SomeSBool sb) f = f sb

toSBool :: Bool -> SomeSBool
toSBool True  = SomeSBool STrue
toSBool False = SomeSBool SFalse

tst1 = let
  b = True
  someSBool = toSBool b
  r = withSomeSBool someSBool foo
  in r

foo :: forall b . SignBool b -> String
foo a = case dict @ShowBool a of
  Dict -> showBool @b



dict :: (c 'True, c 'False) => SignBool b -> Dict (c b)
dict STrue  = Dict
dict SFalse = Dict

-- | It is impossible to implement Show, because usual typeclasses require kind Type,
--   but we have kind Bool
class ShowBool (b :: Bool) where
  showBool :: String
instance ShowBool 'True where showBool = "ST"
instance ShowBool 'False where showBool = "SF"