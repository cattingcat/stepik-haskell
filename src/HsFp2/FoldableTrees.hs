module HsFp2.FoldableTrees where

import Data.Foldable

data Triple a = Tr a a a  deriving (Eq,Show)

instance Foldable Triple where
    -- foldr :: (a -> b -> b) -> b -> Triple a -> b
    -- foldl :: (b -> a -> b) -> b -> Triple a -> b
    foldr f ini (Tr a1 a2 a3) = f a1 (f a2 (f a3 ini)) 
    foldl f ini (Tr a1 a2 a3) = f (f (f ini a1) a2) a3


test11 = foldr (++) "!!" (Tr "ab" "cd" "efg")
test12 = foldl (++) "!!" (Tr "ab" "cd" "efg")




data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch l a r) = Branch (fmap f l) (f a) (fmap f r)

instance Functor Preorder where
    fmap f (PreO t) = PreO $ fmap f t

tree = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
{-

         3
    1         4
  -   2     -   - 

-}

leaf node = Branch Nil node Nil
testTree = (Branch 
        (Branch (leaf "A") "B" 
        (Branch (leaf "C") "D" (leaf "E"))) 
            "F" (Branch Nil "G" (Branch (leaf "H") "I" Nil))) 

test21 = foldr (:) [] tree --[1,2,3,4]
test22 = foldr (:) [] $ PreO tree  --[3,1,2,4]
test23 = foldr (:) [] $ PostO tree  --[2,1,4,3]
test24 = foldr (:) [] $ LevelO tree  --[3,1,4,2]

test25 = foldr (:) [5] $ PreO tree  --[3,1,2,4,5]

test26 = foldr (:) [] $ PreO testTree  
test27 = foldr (:) [] $ PostO testTree  
test28 = foldr (:) [] $ LevelO testTree 


-- sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
-- sequenceA_ tfa = foldr (*>) (pure ()) tfa
-- execute all effects but drop value
tree2 = Branch (Branch Nil 1 Nil) 2 (Branch (Branch Nil 3 Nil) 4 (Branch Nil 5 Nil))
test29 = fst $ sequenceA_ $ (\x -> (show x,x)) <$> PreO tree2

instance Foldable Tree where
    foldr _ ini Nil = ini
    foldr f ini (Branch l v r) = let fr = f v (foldr f ini r) in (foldr f fr l) 

instance Foldable Preorder where
    foldr _ ini (PreO Nil) = ini
    foldr f ini (PreO (Branch l v r)) = let 
        fl i = foldr f i (PreO l)
        fr i = foldr f i (PreO r)
        fv i = f v i 
        in fv $ fl (fr ini)

-- instance Foldable Postorder where
--     foldr _ ini (PostO Nil) = ini
--     foldr f ini (PostO a) = subfoldr True ini a where
--         subfoldr isR ini Nil = ini
--         subfoldr isR ini (Branch l v r) = let 
--             v' = (f v ini) 
--             isR' = not isR
--             in if isR 
--                 then subfoldr isR' (subfoldr isR' v' r) l
--                 else subfoldr isR' (subfoldr isR' v' l) r

instance Foldable Postorder where
    foldr _ ini (PostO Nil) = ini
    foldr f ini (PostO (Branch l v r)) = let 
        fl i = foldr f i (PostO l)
        fr i = foldr f i (PostO r)
        fv i = f v i 
        in fl $ fr (fv ini)


type TreeLevel a = [Tree a]

down :: TreeLevel a -> TreeLevel a
down l = l >>= \i -> case i of 
    Branch Nil              _ Nil               -> []
    Branch a@(Branch _ _ _) _ b@(Branch _ _ _)  -> [a,b]
    Branch Nil              _ b@(Branch _ _ _)  -> [b]
    Branch a@(Branch _ _ _) _ Nil               -> [a]

layers :: Tree a -> [TreeLevel a]
layers Nil = []
layers t = loop [[t]] [t] where
    loop acc l = let downLevel = down l in 
        if null downLevel then acc else loop (acc ++ [downLevel]) downLevel

layeredValues :: Tree a -> [[a]]
layeredValues t = let 
    ls = layers t 
    in fmap (fmap getVal) ls

getVal :: Tree a -> a
getVal ~(Branch _ v _) = v

    

instance Foldable Levelorder where
    foldr _ ini (LevelO Nil) = ini
    foldr f ini (LevelO t) = let
        layersV = layeredValues t
        lst = concat layersV
        in foldr f ini lst