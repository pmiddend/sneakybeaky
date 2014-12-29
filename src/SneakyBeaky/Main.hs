module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Exception.Base(bracket_)
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

data LightSource = LightSource {
    lsPosition :: Coord
  , lsRadius :: Int
  }

data World = World {
    wHero :: Coord
  , wObstacles :: [ObstacleTile]
  , wExit :: Coord
  , wLightSources :: [LightSource]
}

data Input = Up
           | Down
           | Left
           | Right
           | Stand
           | Exit
           deriving (Eq)

lightTiles :: LightSource -> [Coord]
lightTiles ls = let (cx,cy) = lsPosition ls
                    r = lsRadius ls
                    ts = [(x,y) | x <- [cx-r..cx+r],y <- [cy-r..cy+r]]
                in filter (\(x,y) -> (x-cx)*(x-cx) + (cy-y)*(cy-y) <= r) ts

litTiles :: [LightSource] -> [Coord]
litTiles = concatMap lightTiles

gameTitle :: String
gameTitle = "sneakybeaky"

initialWorld :: [ObstacleTile] -> [LightSource] -> Coord -> World
initialWorld obstacles lightSources exit = World {
    wHero = (0,0)
  , wObstacles = obstacles
  , wExit = exit
  , wLightSources = lightSources
  }

randomViewportCoord :: (MonadRandom m) => Coord -> m Coord
randomViewportCoord c = do
  x <- getRandomR (0,(fst c))
  y <- getRandomR (0,(snd c))
  return (x,y)

generateNoConflict :: (MonadRandom m) => Coord -> [Coord] -> m Coord
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

generateObstacles :: MonadRandom m => Coord -> m [ObstacleTile]
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
  gameLoop (initialWorld obstacles [LightSource (10,10) 5] exit)

renderWorld :: World -> [Tile]
renderWorld w = let firstTiles = [renderHero (wHero w),renderExit (wExit w)] ++ map renderObstacle (wObstacles w)
                    lit = litTiles (wLightSources w)
                    renderedLit = map renderLit (filter (\lite -> not (lite `elem` (map tPosition firstTiles))) lit)
                in firstTiles ++ renderedLit

renderLit :: Coord -> Tile
renderLit c = Tile { tCharacter = '.', tSgr = [SetConsoleIntensity BoldIntensity ], tPosition = c }

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
  if wHero w == wExit w then liftIO (putStrLn "You won!") else do
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
    '.' -> return Stand
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
                    _ -> oldCoord
        newCoord = case (obstacleAt w newCoord') of
                    Nothing -> newCoord'
                    Just _ -> oldCoord
