{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ThinkingWithTypes.IxMonad where

import Control.Monad.Indexed
import Data.Coerce

newtype Ix m i j a = Ix { unsafeRunIx :: m a }
  deriving (Functor, Applicative, Monad)

instance Functor m => IxFunctor (Ix m) where
  imap :: (a -> b) -> Ix m j k a -> Ix m j k b
  imap = fmap

instance Applicative m => IxPointed (Ix m) where
  ireturn :: a -> Ix m i i a
  ireturn = pure

instance Applicative m => IxApplicative (Ix m) where
  iap :: forall i j k a b . Ix m i j (a -> b) -> Ix m j k a -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b

instance Monad m => IxMonad (Ix m) where
  ibind :: forall i j k a b . (a -> Ix m j k b) -> Ix m i j a -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b