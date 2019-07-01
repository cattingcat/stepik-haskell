module HsFp2.MonadsAndEffects.ExceptT.GameFieldTask where
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Data.Monoid
import Data.Char


data Tile = Floor | Chasm | Snake deriving Show

data DeathReason = Fallen | Poisoned deriving (Eq, Show)

type Point = (Integer, Integer)
type GameMap = Point -> Tile

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves m s p = runExceptT $ loop s p where 
    loop 1 p = around m p
    loop s p = do
        pi <- around m p
        r <- loop (s-1) pi
        return r

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie r m s p = let 
    steps = moves m s p
    filt (Right _)     = False
    filt (Left reason) = reason == r
    ways = filter filt steps
    in length ways



around :: GameMap -> Point -> ExceptT DeathReason [] Point
around m (x, y) = let 
    moves = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    matchE :: Point -> Either DeathReason Point
    matchE p = case m p of
        Floor -> Right p
        Chasm -> Left Fallen
        Snake -> Left Poisoned
    eithers = fmap matchE moves
    in ExceptT eithers



map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm




tst11 = waysToDie Poisoned map1 1 (4,2) -- 1  -- можно пойти к змее наверх
tst12 = waysToDie Poisoned map1 2 (4,2) -- 2  -- можно пойти к змее наверх или к змее влево
tst13 = waysToDie Poisoned map1 3 (4,2)
-- 5  -- за три шага к левой змее, по-прежнему можно дойти одним способом,
      -- а к правой — уже четырьмя (вверх, влево-вверх-вправо,
      --                            влево-вправо-вверх, вниз-вверх-вверх)
tst14 = waysToDie Poisoned map1 4 (4,2) -- 13