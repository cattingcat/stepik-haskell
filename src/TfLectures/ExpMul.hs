{-# LANGUAGE FlexibleInstances #-}

module TfLectures.ExpMul where

-- | http://okmij.org/ftp/tagless-final/course/lecture.pdf

-- It is impossible to reuse old Exp
data Exp = Lit Int | Neg Exp | Add Exp Exp | Mul Exp Exp
  deriving (Eq)

instance Show Exp where
  show (Lit n) = show n
  show (Neg n) = "-" <> show n
  show (Add e1 e2) = show e1 <> " + " <> show e2
  show (Mul e1@(Add _ _) e2@(Add _ _)) = "(" <> show e1 <> ") * (" <> show e2 <> ")"
  show (Mul e@(Add e0 e1) e2) = "(" <> show e <> ") * " <> show e2
  show (Mul e1 e@(Add e2 e3)) = show e1 <> " * (" <> show e <> ")"
  show (Mul e1 e2) = show e1 <> " * " <> show e2

class ExpFinal repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

class ExpFinal repr => ExpFinalMul repr where
  mul :: repr -> repr -> repr



instance ExpFinal Exp where
  lit = Lit
  neg = Neg
  add = Add

instance ExpFinalMul Exp where
  mul = Mul

finalize :: ExpFinalMul repr => Exp -> repr
finalize (Lit n) = lit n
finalize (Neg e) = neg (finalize e)
finalize (Add e1 e2) = add (finalize e1) (finalize e2)
finalize (Mul e1 e2) = mul (finalize e1) (finalize e2)

instance ExpFinal Int where
  lit = id
  neg a = -a
  add = (+)

instance ExpFinalMul Int where
  mul = (*)


distMul :: Exp -> Exp
distMul e@(Lit _) = e
distMul e@(Neg _) = e
distMul (Add e1 e2) = Add (distMul e1) (distMul e2)
distMul (Mul (Add e1 e2) e3) = Add (distMul $ Mul e1 e3) (distMul $ Mul e2 e3)
distMul (Mul e1 (Add e2 e3)) = Add (distMul $ Mul e1 e2) (distMul $ Mul e1 e3)
distMul (Mul e1 e2) = Mul (distMul e1) (distMul e2)

tst = Mul (Add (Lit 1) (Lit 2)) (Add (Lit 3) (Lit 4))
tst' = distMul tst


data Ctx e = MulCtx e | NonMulCtx

instance ExpFinalMul repr => ExpFinal (Ctx repr -> repr) where
  lit n NonMulCtx = lit n
  lit n (MulCtx e) = mul (lit n) e

  neg e NonMulCtx = neg (e NonMulCtx)
  neg e (MulCtx e1) = mul (neg e NonMulCtx) e1

  add e1 e2 (MulCtx e) = add (e1 (MulCtx e)) (e2 (MulCtx e))
  add e1 e2 NonMulCtx = add (e1 NonMulCtx) (e2 NonMulCtx)

instance ExpFinalMul repr => ExpFinalMul (Ctx repr -> repr) where
  mul e1 e2 ctx = e1 (MulCtx (e2 ctx))




data CtxR e = MulCtxR e | NonMulCtxR

instance ExpFinalMul repr => ExpFinal (CtxR repr -> repr) where
  lit n NonMulCtxR = lit n
  lit n (MulCtxR e) = mul (lit n) e

  neg e NonMulCtxR = neg (e NonMulCtxR)
  neg e (MulCtxR e1) = mul (neg e NonMulCtxR) e1

  add e1 e2 (MulCtxR e) = add (e1 (MulCtxR e)) (e2 (MulCtxR e))
  add e1 e2 NonMulCtxR = add (e1 NonMulCtxR) (e2 NonMulCtxR)

instance ExpFinalMul repr => ExpFinalMul (CtxR repr -> repr) where
  mul e1 e2 ctx = e2 (MulCtxR (e1 ctx))




tst2 :: ExpFinalMul repr => repr
tst2 = mul (mul (add (lit 1) (lit 2)) (add (lit 3) (lit 4))) (add (lit 5) (lit 6))

tst3 :: ExpFinalMul repr => Ctx repr -> repr
tst3 = tst2

tst4 :: ExpFinalMul repr => Ctx (CtxR repr -> repr) -> (CtxR repr -> repr)
tst4 = tst2

tst5 :: Exp
tst5 = tst4 NonMulCtx NonMulCtxR

