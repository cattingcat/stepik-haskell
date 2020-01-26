{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ThinkingWithTypes.DepTypes where

import Control.Monad.Trans.Writer
import Data.Constraint (Dict (..)) -- constraints package
import Data.Foldable (for_)
import Data.Kind (Type)
import Debug.Trace (trace)
import GHC.IO (unsafePerformIO)

data SBool (b :: Bool) where
  STrue :: SBool 'True
  SFalse :: SBool 'False

-- | Isomorphism
fromSBool :: SBool b -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

-- | Backward iso is more tricky
--toSBool :: Bool -> SBool b
--toSBool True = STrue
--toSBool False = SFalse

data SomeSBool where
  SomeSBool :: SBool b -> SomeSBool

withSomeBool :: SomeSBool -> (forall (b :: Bool) . SBool b -> r) -> r
withSomeBool (SomeSBool sbool) f = f sbool

toSomeBool :: Bool -> SomeSBool
toSomeBool True   = SomeSBool STrue
toSomeBool False  = SomeSBool SFalse

tst2 = do
  b <- readLn
  let 
    someBool = toSomeBool b
    bool = withSomeBool someBool fromSBool
  putStrLn (show bool)


tst1 = let
  someBool = toSomeBool True
  bool = withSomeBool someBool fromSBool
  in trace (show bool) 0



class HelloClass (b :: Bool) where
  say :: ()

instance HelloClass 'True where
  say = unsafePerformIO (print "True")

instance HelloClass 'False where
  say = unsafePerformIO (print "False")

tst3 = let
  someBool = toSomeBool True
--  p = withSomeBool someBool (\(_ :: SBool b) -> say @b)
  p = withSomeBool someBool foo
  in 0

-- GHC cant find correct instance
--foo :: HelloClass b => SBool b -> ()
--foo _ = say @b

dict :: (c 'True, c 'False) => SBool b -> Dict (c b)
dict STrue  = Dict
dict SFalse = Dict

foo :: forall b . SBool b -> ()
foo b = case dict @HelloClass b of
  Dict -> say @b 