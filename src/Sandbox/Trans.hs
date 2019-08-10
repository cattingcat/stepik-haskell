module Sandbox.Trans where
import Control.Monad
import Control.Monad.State

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= return . f