module DataStructures.Heap where

data Heap a = E | T Int a (Heap a) (Heap a)

insert :: Ord a => Heap a -> a -> Heap a
insert h a = merge (T 1 a E E) h

hmin :: Heap a -> a
hmin E = undefined
hnim (T _ a _ _) = a

popMin :: Ord a => Heap a -> (a, Heap a)
popMin E = undefined
popMin (T _ a l r) = (a, merge l r)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge E h = h
merge h E = h
merge h1@(T n1 a1 l1 r1) h2@(T n2 a2 l2 r2) = 
  if a1 <= a2 then makeT a1 l1 (merge r1 h2) else makeT a2 l2 (merge h1 r2)

rank :: Heap a -> Int
rank E = 0
rank (T n _ _ _) = n

makeT :: a -> Heap a -> Heap a -> Heap a
makeT a h1 h2 = let 
  rh1 = rank h1
  rh2 = rank h2
  in if rh1 <= rh2 then T (rh1 + 1) a h2 h1 else T (rh2 + 1) a h1 h2 