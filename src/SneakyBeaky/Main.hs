module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Exception.Base(bracket_)
import System.Random
import Control.Monad.Random
import Data.List(find,nub)

type Coord = (Int, Int)

data Tile = Tile {
    tPosition :: Coord
  , tCharacter :: Char
  , tSgr :: [SGR]
  }

data ObstacleTile = ObstacleTile {
    oTile :: Tile
  , oSolid :: Bool
  }

data World = World {
    wHero :: Coord
  , wObstacles :: [ObstacleTile]
  , wExit :: Coord
}

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq)

gameTitle :: String
gameTitle = "sneakybeaky"

initialWorld :: [ObstacleTile] -> Coord -> World
initialWorld obstacles exit = World (0,0) obstacles exit

randomViewportCoord :: RandomGen g => Coord -> Rand g Coord
randomViewportCoord c = do
  x <- getRandomR (0,(fst c))
  y <- getRandomR (0,(snd c))
  return (x,y)

generateNoConflict :: RandomGen g => Coord -> [Coord] -> Rand g Coord
generateNoConflict viewport xs = do
  c <- randomViewportCoord viewport
  if not (c `elem` xs)
    then return c
    else generateNoConflict viewport xs

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
line :: Coord -> Coord -> [Coord]
line (x0, y0) (x1, y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in  walk (balancedWord p q 0) (x0, y0)

obstacleFromCoord :: Coord -> ObstacleTile
obstacleFromCoord pos = ObstacleTile (Tile pos '#' []) True

generateObstacles :: RandomGen g => Coord -> Rand g [ObstacleTile]
generateObstacles dim = do
  let w = fst dim
      h = snd dim
      boxSize = 10
  x1 <- getRandomR (0,w-1)
  let x2 = min (w - 1) (x1 + 5)
  y1 <- getRandomR (0,h-1)
  let y2 = min (h - 1) (y1 + boxSize)
  let dx = x2 - x1 + 1
      dy = y2 - y1 + 1
      p1 = (x1, y1)
      p2 = (x2, y1)
      p3 = (x1, y2)
      p4 = (x2, y2)
  return $ map obstacleFromCoord $ nub $ concat [
    take dx $ line p1 p2,
    take dy $ line p1 p3,
    take dy $ line p2 p4,
    take dx $ line p3 p4]

main :: IO ()
main = bracket_ (hSetEcho stdin False >> hSetBuffering stdin  NoBuffering >> hSetBuffering stdout NoBuffering >> hideCursor) (showCursor >> hSetEcho stdin True) $ do
  let standardViewport = (80,25)
  obstacles <- evalRandIO $ generateObstacles standardViewport
  exit <- evalRandIO (generateNoConflict standardViewport (map (tPosition . oTile) obstacles))
  setTitle gameTitle
  gameLoop (initialWorld obstacles exit)

renderWorld :: World -> [Tile]
renderWorld w = [renderHero (wHero w),renderExit (wExit w)] ++ map renderObstacle (wObstacles w)

renderHero :: Coord -> Tile
renderHero c = Tile { tCharacter = '@', tSgr = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ], tPosition = c }

renderExit :: Coord -> Tile
renderExit c = Tile {
    tCharacter = '>'
  , tSgr = [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue
           ]
  , tPosition = c
  }

renderObstacle :: ObstacleTile -> Tile
renderObstacle = oTile

renderGame :: MonadIO m => World -> m ()
renderGame w = do
  liftIO clearScreen
  mapM_ drawTile (renderWorld w)

gameLoop :: MonadIO m => World -> m ()
gameLoop w = do
  renderGame w
  if wHero w == wExit w then liftIO (putStrLn "You won!") >> return () else do
    input <- getInput
    case input of
      Exit -> return ()
      _    -> gameLoop (handleDir w input)

drawTile :: MonadIO m => Tile -> m ()
drawTile t = do
  liftIO $ setCursorPosition (snd (tPosition t)) (fst (tPosition t))
  liftIO $ setSGR (tSgr t)
  liftIO $ putStr [tCharacter t]

drawHero :: MonadIO m => Coord -> m ()
drawHero c = drawTile $ Tile {
    tCharacter = '@'
  , tSgr = [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue
           ]
  , tPosition = c
  }

getInput :: MonadIO m => m Input
getInput = do
  char <- liftIO getChar
  case char of
    'q' -> return Exit
    'k' -> return Up
    'j' -> return Down
    'h' -> return Left
    'l' -> return Right
    _ -> getInput

obstacleAt :: World -> Coord -> Maybe ObstacleTile
obstacleAt w c = find ((== c) . tPosition . oTile) (wObstacles w)

handleDir :: World -> Input -> World
handleDir w input = w { wHero = newCoord }
  where oldCoord@(heroX,heroY) = wHero w
        newCoord' = case input of
                    Up    -> (heroX, heroY - 1)
                    Down  -> (heroX, heroY + 1)
                    Left  -> (heroX - 1, heroY)
                    Right -> (heroX + 1, heroY)
        newCoord = case (obstacleAt w newCoord') of
                    Nothing -> newCoord'
                    Just _ -> oldCoord
