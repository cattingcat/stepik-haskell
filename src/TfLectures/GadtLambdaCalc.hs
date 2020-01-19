{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module TfLectures.GadtLambdaCalc where

data Var env t where
  VZ :: Var (t, env) t
  VS :: Var env t -> Var (a, env) t


-- Var represents structure like (a, (b, (c, ...))) for env

data Exp env t where
  B :: Bool             -> Exp env Bool             -- Bool type              -- Booleans axiom
  V :: Var env t        -> Exp env t                -- Vars                   -- Hypotesis
  L :: Exp (a, env) b   -> Exp env (a -> b)         -- Abstraction (lambda)   -- implication introduction
  A :: Exp env (a -> b) -> Exp env a -> Exp env b   -- Application            -- implication elimination

lookp :: Var env t -> env -> t
lookp VZ (t, _) = t
lookp (VS v) (_, env) = lookp v env

eval :: env -> Exp env t -> t
eval env (B b) = b
eval env (V v) = lookp v env
eval env (L e) = \x -> eval (x, env) e
eval env (A e1 e2) = (eval env e1) (eval env e2)

ti1 = A (L (V VZ)) (B True)

tst1 = eval () ti1



-- | http://okmij.org/ftp/tagless-final/course/TTFdB.hs
-- | final:
-- | Index based lambda calculus (see s and z)

class Symantics repr where
  int :: Int -> repr h Int
  add :: repr h Int -> repr h Int -> repr h Int

  z :: repr (a, h) a
  s :: repr h a -> repr (any, h) a

  lam :: repr (a, h) b -> repr h (a -> b)       -- if E has type B in context (a, h), then lam E has type a -> b in context h
  app :: repr h (a -> b) -> repr h a -> repr h b

-- examples:
td1 :: Symantics repr => repr h Int
td1 = add (int 1) (int 2)

td2 :: Symantics repr => repr (Int, h) Int
td2 = add z z

td2' :: Symantics repr => repr h (Int -> Int)
td2' = lam (add z z)

td2o :: Symantics repr => repr (Int, h) (Int -> Int)
td2o = lam (add z (s z))



newtype R h a = R { unR :: h -> a }

instance Symantics R where
  int n = R $ const n
  add e1 e2 = R $ \h -> (unR e1 h) + (unR e2 h)
  z = R $ \(x, _) -> x
  s v = R $ \(_, h) -> unR v h
  lam e = R $ \h -> (\x -> (unR e) (x, h))
  app e1 e2 = R $ \h -> (unR e1 h) (unR e2 h)

tst2 :: R () Int
tst2 = td1

tst3 :: R () Int
tst3 = app (lam (app (lam (add z (s z))) (z))) (int 1)

tst4 = app (app (lam (lam (add z (s z)))) (int 2)) (int 3)

evalR e = unR e ()


newtype S h a = S { unS :: Int -> String }

instance Symantics S where
  int n = S $ const (show n)
  add e1 e2 = S $ \h -> "(" <> (unS e1 h) <> "+" <> (unS e2 h) <> ")"
  z = S $ \h -> "x" <> show (h - 1)
  s v = S $ \h -> unS v (h - 1)
  lam e = S $ \h ->
    let x = "x" <> show h
    in "(\\" <> x <> " -> " <> unS e (h + 1) <> ")"
  app e1 e2 = S $ \h -> "(" <> (unS e1 h) <> (unS e2 h) <> ")"

evalS e = unS e 0