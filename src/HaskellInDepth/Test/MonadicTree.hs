{-# LANGUAGE TypeSynonymInstances #-}

module HaskellInDepth.Test.MonadicTree where

import Prelude hiding (div)


data HtmlTree = Node {
  tag :: String,
  attrs :: [String],
  nodes :: [HtmlTree]
} deriving (Show)

emptyTree :: HtmlTree
emptyTree =  Node "root" [] []

data MonadicTree a = Nd (HtmlTree, a) | SubList (HtmlTree, a)
  deriving (Show)

instance Functor MonadicTree where
  fmap f (Nd (t, a)) = Nd (t, f a)
  fmap f (SubList (t, a)) = SubList (t, f a)

instance Applicative MonadicTree where
  pure a = SubList (emptyTree, a)
  (<*>) = undefined

instance Monad MonadicTree where
  return a = SubList (emptyTree, a)
  (>>=) (Nd (Node name as cs, a)) f =
    case f a of
      (Nd (n@(Node _ as' cs'), a')) -> Nd (Node name as (cs <> [n]), a')
      (SubList (Node _ as' cs', a')) -> Nd (Node name (as <> as') (cs <> cs'), a')



html :: MonadicTree () -> MonadicTree ()
html (Nd (child, _)) = Nd (Node "html" [] [child], ())
--html (SubList (child, _)) = Nd (Node "html" [] [child], ())

div :: MonadicTree () -> MonadicTree ()
div (SubList (Node _ as cs, _)) = Nd (Node "div" as cs, ())
div (Nd (n@(Node _ as cs), _)) = Nd (Node "div" [] [n], ())

attr :: String -> MonadicTree ()
attr s = SubList (Node "attr" [s] [], ())

tst1 = div $ do
  div $ pure ()
  attr "kek"