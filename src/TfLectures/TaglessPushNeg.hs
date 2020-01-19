{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module TfLectures.TaglessPushNeg where

-- | http://okmij.org/ftp/tagless-final/course/lecture.pdf

data Exp = Lit Int | Add Exp Exp | Neg Exp
  deriving (Show, Eq)


pushNeg :: Exp -> Exp
pushNeg e@(Neg (Lit n)) = e
pushNeg   (Neg (Add e1 e2)) = Add (pushNeg $ Neg e1) (pushNeg $ Neg e2)
pushNeg   (Neg (Neg e)) = e
pushNeg e@(Lit n) = e
pushNeg   (Add e1 e2) = Add (pushNeg e1) (pushNeg e2)

tst = Add (Neg (Lit 5)) (Neg (Add (Lit 3) (Neg (Lit 4)))) --   -5 + -(3 + (-4))
tst' = pushNeg tst -- -5 + (-3 + 4)


class ExpFinal repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

data Ctx = P | N -- positive/negative

instance ExpFinal repr => ExpFinal (Ctx -> repr) where
    lit :: Int -> (Ctx -> repr)
    lit n P = lit n
    lit n N = neg (lit n)

    neg :: (Ctx -> repr) -> (Ctx -> repr)
    neg e P = e N
    neg e N = e P

    add :: (Ctx -> repr) -> (Ctx -> repr) -> (Ctx -> repr)
    add e1 e2 ctx = add (e1 ctx) (e2 ctx)



instance ExpFinal Exp where
  lit = Lit
  neg = Neg
  add = Add

finalize :: ExpFinal repr => Exp -> repr
finalize (Lit n) = lit n
finalize (Neg e) = neg (finalize e)
finalize (Add e1 e2) = add (finalize e1) (finalize e2)



instance ExpFinal Int where
  lit = id
  neg a = -a
  add = (+)

tst2 :: ExpFinal a => a
tst2 = add (neg (lit 5)) (neg (add (lit 3) (neg (lit 4))))

-- tst2   :: Exp - interpret as Exp
-- tst2   :: Int - interpret as Int
-- tst2 P :: Int - interpret as Ctx -> a THEN push neg THEN interpret as Int

tst2' = tst' == tst2 P



flatten :: Exp -> Exp
flatten e@(Lit _) = e
flatten e@(Neg _) = e
flatten (Add (Add e1 e2) e3) = flatten $ Add e1 (Add e2 e3)
flatten (Add e1 e2)          = Add e1 (flatten e2)



data FCtx e = LeftChildAdd e | NotLCA

instance ExpFinal repr => ExpFinal (FCtx repr -> repr) where
  lit :: Int -> (FCtx repr -> repr)
  lit n NotLCA = lit n
  lit n (LeftChildAdd lca) = add (lit n) lca
  neg :: (FCtx repr -> repr) -> (FCtx repr -> repr)
  neg e NotLCA = neg (e NotLCA)
  neg e (LeftChildAdd lca) = add (neg (e NotLCA)) lca
  add :: (FCtx repr -> repr) -> (FCtx repr -> repr) -> (FCtx repr -> repr)
  add e1 e2 ctx = e1 (LeftChildAdd (e2 ctx)) -- We say that e1 is LCA and pass ctx

tst3 :: ExpFinal a => a
tst3 = add
  (add
    (add
      (add
        (lit 1)
        (lit 2))
      (lit 3))
    (lit 4))
  (lit 5)

tst3' :: Exp
tst3' = (tst3 :: FCtx Exp -> Exp) NotLCA

--  TODO: Multiplication