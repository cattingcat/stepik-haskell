{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ThinkingWithTypes.IndexedMonad where

import Control.Monad.Indexed
import Data.Coerce
import Fcf
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as TL
import ThinkingWithTypes.IxMonad
import Language.Haskell.DoNotation
import Prelude hiding (Monad (..), pure)
import qualified System.IO as SIO
import System.IO hiding (openFile, Handle)

-- | Linear types emulation

data LinearState = Ls {
  next :: TL.Nat,
  opened :: [TL.Nat]
}

newtype Linear s (i :: LinearState) (j :: LinearState) a = Linear { unsafeRunLinear :: Ix IO i j a }
  deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)

newtype Handle s (n :: Nat) = Handle { unsafeGetHandle :: SIO.Handle }

-- | s - ST-trick. It is impossible to use Handle outside of monad see ThinkingWithTypes3.hs

openFile :: FilePath -> IOMode -> Linear s ('Ls n ops) ('Ls (n TL.+ 1) (n ': ops)) (Handle s n)
openFile = coerce SIO.openFile


type IsOpened (n :: Nat) (ns :: [Nat]) = Eval (IsJust =<< (Find (TyEq n) ns))
type Close (n :: Nat) (ns :: [Nat]) = Eval (Filter (Not <=< (TyEq n)) ns)

closeFile :: ((IsOpened nh ops) ~ 'True) => Handle s nh -> Linear s ('Ls n ops) ('Ls n (Close nh ops)) ()
closeFile = coerce SIO.hClose

runLinear :: forall s n a . Linear s ('Ls 0 '[]) ('Ls n '[]) a -> IO a
runLinear = coerce . unsafeRunLinear

tst = runLinear $ do
  h <- openFile "/etc/passwd" ReadMode
  closeFile h