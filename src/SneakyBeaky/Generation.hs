module SneakyBeaky.Generation where

import SneakyBeaky.Coord
import Prelude hiding (Either(..))
-- import System.Console.ANSI
import System.IO
import Control.Monad(replicateM,liftM)
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Exception.Base(bracket_)
import Data.Monoid((<>),mconcat)
import Control.Monad.Random
import Data.List(find,nub,(\\))
import qualified Data.Set as Set
--import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Map
import Data.Maybe(isNothing)
import SneakyBeaky.TileTypes
import SneakyBeaky.Rect
import SneakyBeaky.Matrix

type CoordSet = Set.Set Coord

clamp :: Ord a => (a, a) -> a -> a
clamp (left, right) x =
  min right (max left x)

clampRect :: Rect -> Coord -> Coord
clampRect bounds (x, y) =
  let (x0, y0) = rTopLeft bounds
      (x1, y1) = rBottomRight bounds
  in (clamp (x0, x1) x, clamp (y0, y1) y)

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

line :: Coord -> Coord -> [Coord]
line start end = (takeWhile (/= end) $ line' start end) ++ [end]

-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
line' :: Coord -> Coord -> [Coord]
line' (x0, y0) (x1, y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in  walk (balancedWord p q 0) (x0, y0)

randomViewportCoord :: (MonadRandom m) => Rect -> m Coord
randomViewportCoord c = do
  let (x0, y0) = rTopLeft c
      (x1, y1) = rBottomRight c
  x <- getRandomR (x0, x1)
  y <- getRandomR (y0, y1)
  return (x,y)

generateNoConflict :: (MonadRandom m) => Rect -> [Coord] -> m Coord
generateNoConflict viewport xs = do
  c <- randomViewportCoord viewport
  if c `notElem` xs
    then return c
    else generateNoConflict viewport xs

obstacleFromCoord :: Coord -> ObstacleTile
obstacleFromCoord pos = ObstacleTile (Tile pos '#' (SneakyColorPair White Transparent)) True

generateObstacle :: MonadRandom m => Rect -> m [ObstacleTile]
generateObstacle bounds = do
  let (w, h) = rDim bounds
      (left, top) = rTopLeft bounds
  x1 <- getRandomR (left, w-1)
  y1 <- getRandomR (top, h-1)
  angle <- getRandomR ((0,3) :: (Int, Int))
  let boxSize = 5
      x2 = x1 + boxSize
      y2 = y1 + boxSize
      p1 = (x1, y1)
      p2 = (x2, y1)
      p3 = (x1, y2)
      p4 = (x2, y2)
      center = cog [p1, p2, p3, p4]
      transform = (`pairPlus` center) . (mMultiply (mRotationMatrix ((pi/8) * fromIntegral angle))) . (`pairMinus` center)
      [q1, q2, q3, q4] = map transform [p1, p2, p3, p4]
      obstacles = nub $ concat [
        line q1 q2,
        line q1 q3,
        line q2 q4,
        line q3 q4]
  return $ map obstacleFromCoord $ filter (insideRect bounds) $ Set.toList $ floodFill center (Set.fromList obstacles)

generateObstacles :: MonadRandom m => Rect -> m [ObstacleTile]
generateObstacles bounds = liftM concat $ replicateM 10 $
  generateObstacle bounds

floodFill :: Coord -> CoordSet -> CoordSet
floodFill start obstacles | start `Set.member` obstacles  = obstacles
                          | otherwise = mconcat $ map step [(0, 1),(1, 0),(-1, 0),(0, -1)]
                          where step c = floodFill (start `pairPlus` c) (Set.insert start obstacles)
