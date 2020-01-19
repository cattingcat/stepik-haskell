module TfLectures.SimpleLambdaCalc where

data Var = VZ | VS Var  -- var id
  deriving (Show, Eq)
data Exp = V Var        -- variable
         | B Bool       -- bool literal
         | L Exp        -- lambda abstraction
         | A Exp Exp    -- application
 deriving (Show, Eq)

ti1 = A (L (V VZ)) (B True)
ti2a = A (B True) (B False) -- type mismatch
ti2o = A (L (V (VS VZ))) (B True) -- var not found


data U = UB Bool | UA (U -> U)

instance Show U where
  show (UB b) = show b
  show (UA _) = "func"

lookp :: Var -> [a] -> a
lookp VZ (x:_) = x
lookp (VS i) (x:xs) = lookp i xs
lookp _ _ = error "missing variable"

eval :: [U] -> Exp -> U
eval env (V v) = lookp v env
eval env (B b) = UB b
eval env (L e) = UA $ \x -> eval (x:env) e
eval env (A e1 e2) = case eval env e1 of
  UA f -> f (eval env e2)
  _ -> error "type mismatch"
  
  
--type ErrorMsg = String
--typecheck :: Exp -> Either ErrorMsg Exp
--typecheck = undefined
 