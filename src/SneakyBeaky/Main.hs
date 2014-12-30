module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Monad(replicateM,liftM)
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Exception.Base(bracket_)
import Control.Monad.Random
import Data.List(find,nub,(\\))
import Data.Maybe(isNothing)

type Coord = (Int, Int)
type Rect = (Coord, Coord)

data Tile = Tile {
    tPosition :: Coord
  , tCharacter :: Char
  , tSgr :: [SGR]
  } deriving(Eq)

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
  x <- getRandomR (0,fst c)
  y <- getRandomR (0,snd c)
  return (x,y)

generateNoConflict :: (MonadRandom m) => Coord -> [Coord] -> m Coord
generateNoConflict viewport xs = do
  c <- randomViewportCoord viewport
  if c `notElem` xs
    then return c
    else generateNoConflict viewport xs

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

insideBounds :: Rect -> Coord -> Bool
insideBounds ((x0, y0), (x1, y1)) (x, y) =
  x0 <= x && x <= x1 &&
  y0 <= y && y <= y1

obstacleFromCoord :: Coord -> ObstacleTile
obstacleFromCoord pos = ObstacleTile (Tile pos '#' []) True

generateObstacle :: MonadRandom m => Coord -> m [ObstacleTile]
generateObstacle dim = do
  let w = fst dim
      h = snd dim
  x1 <- getRandomR (0, w-1)
  y1 <- getRandomR (0, h-1)
  let boxSize = 5
      x2 = (x1 + boxSize)
      y2 = (y1 + boxSize)
  return $ map obstacleFromCoord $ filter (insideBounds ((0,0),dim)) $ nub $
    [(x,y) | x <- [x1..x2], y <- [y1..y2]]

generateObstacles :: MonadRandom m => Coord -> m [ObstacleTile]
generateObstacles dim = liftM concat $ replicateM 10 $
  generateObstacle dim

main :: IO ()
main = bracket_ (hSetEcho stdin False >> hSetBuffering stdin  NoBuffering >> hSetBuffering stdout NoBuffering >> hideCursor) (showCursor >> hSetEcho stdin True) $ do
  let standardViewport = (80,25)
  obstacles <- evalRandIO $ generateObstacles standardViewport
  exit <- evalRandIO (generateNoConflict standardViewport (map (tPosition . oTile) obstacles))
  setTitle gameTitle
  liftIO clearScreen
  gameLoop [] (initialWorld obstacles [LightSource (10,10) 200] exit)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

insideLight :: Coord -> LightSource -> Bool
insideLight (x,y) (LightSource (lx,ly) r) = (x-lx)*(x-lx) + (y-ly)*(y-ly) <= r

viewObstructed :: [Coord] -> LightSource -> Coord -> Maybe Coord
viewObstructed obstacles light x = find (`elem` obstacles) (line (lsPosition light) x)

tileDiff :: MonadIO m => [Tile] -> [Tile] -> m ()
tileDiff before after = do
  let toClear = before \\ after
      toAdd = after \\ before
  mapM_ clearTile toClear
  mapM_ drawTile toAdd

renderWorld :: World -> [Tile]
renderWorld w = let obstacleTiles = map renderObstacle (wObstacles w)
                    realTiles = [renderHero (wHero w),renderExit (wExit w)] ++ obstacleTiles
                    lit = litTiles (wLightSources w)
                    litFilter lite = lite `notElem` map tPosition realTiles &&
                                     isNothing (viewObstructed (map tPosition obstacleTiles) (head (wLightSources w)) lite)
                    renderedLit = map renderLit . filter litFilter $ lit
                in realTiles ++ renderedLit

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

gameLoop :: MonadIO m => [Tile] -> World -> m ()
gameLoop prevTiles w = do
  let thisTiles = renderWorld w
  tileDiff prevTiles thisTiles
  --mapM_ drawTile thisTiles
  if wHero w == wExit w then liftIO (putStrLn "You won!") else do
    input <- getInput
    case input of
      Exit -> return ()
      _    -> gameLoop thisTiles (handleDir w input)

drawTile :: MonadIO m => Tile -> m ()
drawTile t = do
  liftIO $ setCursorPosition (snd (tPosition t)) (fst (tPosition t))
  liftIO $ setSGR (tSgr t)
  liftIO $ putStr [tCharacter t]

clearTile :: MonadIO m => Tile -> m ()
clearTile t = do
  liftIO $ setCursorPosition (snd (tPosition t)) (fst (tPosition t))
  liftIO $ putStr " "

drawHero :: MonadIO m => Coord -> m ()
drawHero c = drawTile Tile {
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
        newCoord = case obstacleAt w newCoord' of
                    Nothing -> newCoord'
                    Just _ -> oldCoord
