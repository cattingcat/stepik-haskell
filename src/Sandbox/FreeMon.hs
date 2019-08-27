{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Sandbox.FreeMon where

import Control.Monad

data Expr = Lit Int | Add Expr Expr

tst1 = Add (Lit 3) (Add (Lit 1) (Lit 2))


-- Different interpreters

eval :: Expr -> Int
eval (Lit n) = n
eval (Add a b) = eval a + eval b

evalTxt :: Expr -> String
evalTxt (Lit n) = show n
evalTxt (Add a b) = evalTxt a ++ evalTxt b



{-
  Say "Hello" .
  Say "Who are you?" .
  Ask for a “ name ”.
  Say "Nice to meet you, " ++ name ++ "!" .
-}

{-
  do
    say "Hello"
    say "Who are you?"
    name ← ask
    say ("Nice to meet you, " ++ name ++ "!")
-}

{-
  say :: String -> Monad ()
  ask :: Monad String
-}

{-
  data Interaction a where
  instance Monad Interaction where
-}

data Interaction :: * -> * where
  Say :: String -> Interaction ()
  Ask :: Interaction String
  Return :: a -> Interaction a
  Bind :: Interaction a -> (a -> Interaction b) -> Interaction b

instance Monad Interaction where
  return = Return
  (>>=) = Bind

instance Applicative Interaction where
  pure = return
  (<*>) = ap

instance Functor Interaction where
  fmap = liftM

say = Say
ask = Ask

tst2 = do
  say "Hello"
  say "Who are you?"
  name <- ask
  say ("Nice to meet you, " ++ name ++ "!")


tst21 = do
  let qa question = do say question; ask
  x <- qa "Tell me..."
  y <- qa "Tell me more..."
  pure (x, y)


tst22 = do
  say "Tell me..."
  x <- ask
  say "Tell me more..."
  y <- ask
  pure (x, y)

runInteractionIo :: Interaction a -> IO a
runInteractionIo (Say s)    = putStrLn s
runInteractionIo Ask        = getLine
runInteractionIo (Return a) = pure a
runInteractionIo (Bind a f) = do
  av <- runInteractionIo a
  let b = f av
  runInteractionIo b


-- -- --
say' :: String -> (() -> Interaction a) -> Interaction a
say' = Bind . Say

ask' :: (String -> Interaction b) -> Interaction b
ask' = Bind Ask

-- -- --

data Interaction2 :: * -> * where
  Say2 :: String -> Interaction2 ()
  Ask2 :: Interaction2 String
  Return2 :: a -> Interaction2 a
  Bind2 :: Interaction2 a -> (a -> Interaction2 b) -> Interaction2 b
  Say2' :: String -> (() -> Interaction2 a) -> Interaction2 a
  Ask2' :: (String -> Interaction2 b) -> Interaction2 b

-- -- --


data Interaction3 :: * -> * where
  Return3 :: a -> Interaction3 a
  Say3 :: String -> (() -> Interaction3 a) -> Interaction3 a
  Ask3 :: (String -> Interaction3 b) -> Interaction3 b

-- data Interaction4 a = Return4 a | Say4 String (() -> Interaction3 a) | Ask4 (String -> Interaction3 a)

instance Monad Interaction3 where
  return = Return3
  (>>=) (Return3 a) f = f a
  (>>=) (Say3 s g) f = Say3 s ((>>= f) . g)
  (>>=) (Ask3 g) f = Ask3 ((>>= f) . g)

instance Applicative Interaction3 where
  pure = return
  (<*>) = ap

instance Functor Interaction3 where
  fmap = liftM

say3 :: String -> Interaction3 ()
say3 s = Say3 s Return3

ask3 :: Interaction3 String
ask3 = Ask3 Return3

runInteraction3 :: Interaction3 a -> IO a
runInteraction3 (Return3 a) = pure a
runInteraction3 (Say3 s f)  = do putStrLn s; runInteraction3 (f ())
runInteraction3 (Ask3 f)    = do s <- getLine; runInteraction3 (f s)

simulate :: Interaction3 a -> [String] -> [String]
simulate (Return3 _) xs  = xs
simulate (Say3 s f) xs   = s : simulate (f ()) xs
simulate (Ask3 f) (x:xs) = simulate (f x) xs

tst3 = do
  say3 "Tell me..."
  x <- ask3
  say3 "Tell me more..."
  y <- ask3
  pure (x, y)

tst32 = simulate tst3 ["Kek", "Puk"]

-- -- -- --

data Interaction4 :: * -> * where
  Return4 :: a -> Interaction4 a
  Wrap4 :: Interaction4Op a -> Interaction4 a

data Interaction4Op :: * -> * where
  Say4 :: String -> (() -> Interaction4 a) -> Interaction4Op a
  Ask4 :: (String -> Interaction4 b) -> Interaction4Op b


-- -- -- --


data Interaction5 :: * -> * where
  Return5 :: a -> Interaction5 a
  Wrap5 :: Interaction5Op (Interaction5 a) -> Interaction5 a

data Interaction5Op :: * -> * where
  Say5 :: String -> (() -> a) -> Interaction5Op a
  Ask5 :: (String -> a) -> Interaction5Op a


-- -- -- --


data Free (r :: * -> *) (a :: *) where
  ReturnF :: a -> Free r a
  WrapF :: r (Free r a) -> Free r a

instance Functor f =>  Monad (Free f) where
  return = ReturnF
  (>>=) (ReturnF a) f = f a
  (>>=) (WrapF a) f = WrapF $ fmap (>>= f) a

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

instance Functor f => Functor (Free f) where
  fmap = liftM



data InteractionOp :: * -> * where
  SayOp :: String -> (() -> a) -> InteractionOp a
  AskOp :: (String -> a) -> InteractionOp a

instance Functor InteractionOp where
  fmap f (SayOp s g) =  SayOp s (f . g)
  fmap f (AskOp g) =  AskOp (f . g)

type InteractionFree a = Free InteractionOp a

sayF :: String -> InteractionFree ()
sayF s = WrapF $ SayOp s ReturnF

askF :: InteractionFree String
askF = WrapF $ AskOp ReturnF

runInteractionFree :: InteractionFree a -> IO a
runInteractionFree (ReturnF a) = pure a
runInteractionFree (WrapF (SayOp s f))  = do putStrLn s; runInteractionFree (f ())
runInteractionFree (WrapF (AskOp f))    = do l <- getLine; runInteractionFree (f l)

tstF = do
  sayF "Tell me..."
  x <- askF
  sayF "Tell me more..."
  y <- askF
  pure (x, y)