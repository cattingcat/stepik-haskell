{-# LANGUAGE RankNTypes #-}
module Sandbox.ForAllTest where


foo :: (forall a. a -> String) -> Int -> String
foo f i = f i