module DataStructures.Tree where

import Debug.Trace

data Tree a = E | T (Tree a) a (Tree a)

contains :: Ord a => Tree a -> a -> Bool
contains E _ = False
contains (T l tv r) v = if trace "cmp" (v <= tv) then contInt tv l else contains r v where 
--  contInt :: a -> Tree a -> Bool
  contInt candidate E = candidate == v
  contInt candidate (T il itv ir) = if trace "cmp" (v <= itv) then contInt itv il else contains ir v
  
  
testTree = T (T (T E 2 E) 3 (T E 4 E)) 5 (T (T E 35 E) 60 (T (T E 65 E) 70 (T E 75 E)))
  
tst1 = contains testTree 5


makeCompleteTree :: a -> Int -> Tree a
makeCompleteTree _ 0 = E
makeCompleteTree a d = let t = makeCompleteTree a (d - 1) in T t a t