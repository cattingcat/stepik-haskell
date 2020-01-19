{-# LANGUAGE ScopedTypeVariables #-}
module TfLectures.AbsLambdaCalc where

class Symantics repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int

  lam :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b

th1 :: (Symantics repr) => repr Int
th1 = add (int 1) (int 2)

th2 :: (Symantics repr) => repr (Int -> Int)
th2 = lam (\x -> add x x)

th3 :: (Symantics repr) => repr ((Int -> Int) -> Int)
th3 = lam (\x -> add (app x (int 1)) (int 2))


newtype R a = R { unR :: a }
  deriving (Show)

instance Symantics R where
  int = R
  add (R a) (R b) = R $ a + b
  lam f = R $ unR . f . R
  app (R f) (R a) = R $ f a

tst1 = th1 :: R Int

tst2 :: Symantics repr => repr Int
tst2 = app th2 (int 5)

evalR :: R a -> a
evalR = unR


type VarCounter = Int
newtype S a = S { unS :: VarCounter -> String }

instance Symantics S where
--   int :: Int -> repr Int
  int n = S $ \_ -> show n
--   add :: repr Int -> repr Int -> repr Int
  add (S f1) (S f2) = S $ \n -> f1 n <> "+" <> f2 n
--
--   lam :: (repr a -> repr b) -> repr (a -> b)
  lam f = S $ \n ->
    let x = "x" <> show n
    in "(\\" <> x <> " -> " <> unS (f (S $ const x)) (succ n) <> ")"
--   app :: repr (a -> b) -> repr a -> repr b
  app (S e1) (S e2) = S $ \n -> "(" <> e1 n <> e2 n <> ")"

evalS :: S a -> String
evalS s = unS s 0



-- | Extensions:

class MulSym repr where
  mul :: repr Int -> repr Int -> repr Int

class BoolSym repr where
  bool :: Bool -> repr Bool
  leq :: repr Int -> repr Int -> repr Bool
  if_ :: repr Bool -> repr a -> repr a -> repr a

class FixSym repr where
  fix :: (repr a -> repr a) -> repr a

inc :: Symantics repr => repr (Int -> Int)
inc = lam (\x -> add x (int 1))

dec :: Symantics repr => repr (Int -> Int)
dec = lam (\x -> add x (int -1))

tpow :: (Symantics repr, MulSym repr, BoolSym repr, FixSym repr) => repr (Int -> Int -> Int)
tpow = lam (\x -> fix (\(self :: repr (Int -> Int)) -> lam (\n ->
  if_
    (leq n (int 0))
    (int 1)
    (mul x (app self (app dec n)))
  )))

tpow7 :: (Symantics repr, MulSym repr, BoolSym repr, FixSym repr) => repr (Int -> Int)
tpow7 = lam (\x -> app (app tpow x) (int 7))

tpow72 :: (Symantics repr, MulSym repr, BoolSym repr, FixSym repr) => repr Int
tpow72 = app tpow7 (int 2)



instance MulSym R where
  mul (R a) (R b) = R $ a * b

instance BoolSym R where
  bool = R
  leq (R a) (R b) = R $ a <= b
  if_ (R b) (R at) (R af) = R $ if b then at else af

instance FixSym R where
  fix f = R $ fx (unR . f . R) where fx f = f (fx f)


instance MulSym S where
  mul (S a) (S b) = S $ \n -> "(" <> a n <> "*" <> b n <> ")"

instance BoolSym S where
  bool b = S $ \_ -> show b
  leq (S a) (S b) = S $  \n -> "(" <> a n <> "<=" <> b n <> ")"
  if_ (S b) (S at) (S af) = S $ \n -> "if " <> b n <> " then " <> at n <> " else " <> af n

instance FixSym S where
  fix e = S $ \n ->
    let self = "self" <> show n
    in "(fix " <> self <> " ." <> unS (e (S $ const self)) (succ n) <> ")"


--evalR tpow72
--evalS tpow72