module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Monad(replicateM,liftM)
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Exception.Base(bracket_)
import Data.Monoid((<>),mconcat)
import Control.Monad.Random
import Data.List(find,nub,(\\))
import qualified Data.HashSet as Set
--import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Map
import Data.Maybe(isNothing)
import SneakyBeaky.Coord
import SneakyBeaky.Rect

type CoordSet = Set.HashSet Coord

data Tile = Tile {
    tPosition :: !Coord
  , tCharacter :: !Char
  , tSgr :: ![SGR]
  } deriving(Eq)

data ObstacleTile = ObstacleTile {
    oTile :: !Tile
  , oSolid :: !Bool
  }

data LightSource = LightSource {
    lsPosition :: !Coord
  , lsRadius :: !Int
  }

data World = World {
    wHero :: !Coord
  , wObstacles :: ![ObstacleTile]
  , wExit :: !Coord
  , wLightSources :: ![LightSource]
  , wViewport :: !Rect
  }

data Input = Up
           | Down
           | Left
           | Right
           | Stand
           | Exit
           | UpLeft
           | UpRight
           | DownLeft
           | DownRight
           deriving (Eq)

lightTiles :: LightSource -> CoordSet
lightTiles ls = let (cx,cy) = lsPosition ls
                    r = lsRadius ls
                    ts = [(x,y) | x <- [cx-r..cx+r],y <- [cy-r..cy+r]]
                in Set.fromList $ filter (\(x,y) -> (x-cx)*(x-cx) + (cy-y)*(cy-y) <= r) ts

litTiles :: [LightSource] -> CoordSet
litTiles = Set.unions . map lightTiles

gameTitle :: String
gameTitle = "sneakybeaky"

initialWorld :: [ObstacleTile] -> [LightSource] -> Coord -> Rect -> World
initialWorld obstacles lightSources exit viewport = World {
    wHero = (0,0)
  , wObstacles = obstacles
  , wExit = exit
  , wLightSources = lightSources
  , wViewport = viewport
  }

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

clamp :: Ord a => (a, a) -> a -> a
clamp (left, right) x =
  min right (max left x)

clampRect :: Rect -> Coord -> Coord
clampRect bounds (x, y) =
  let (x0, y0) = rTopLeft bounds
      (x1, y1) = rBottomRight bounds
  in (clamp (x0, x1) x, clamp (y0, y1) y)

insideBounds :: Rect -> Coord -> Bool
insideBounds bounds (x, y) =
  let (x0, y0) = rTopLeft bounds
      (x1, y1) = rBottomRight bounds
  in x0 <= x && x <= x1 && y0 <= y && y <= y1

obstacleFromCoord :: Coord -> ObstacleTile
obstacleFromCoord pos = ObstacleTile (Tile pos '#' []) True

generateObstacle :: MonadRandom m => Rect -> m [ObstacleTile]
generateObstacle bounds = do
  let (w, h) = rDim bounds
      (left, top) = rTopLeft bounds
  x1 <- getRandomR (left, w-1)
  y1 <- getRandomR (top, h-1)
  let boxSize = 4
      x2 = (x1 + boxSize)
      y2 = (y1 + boxSize)
      obstacles = nub $ concat [
        line (x1, y1) (x2, y1),
        line (x1, y1) (x1, y2),
        line (x2, y1) (x1, y2)]
  return $ map obstacleFromCoord $ filter (insideBounds bounds) $ (Set.toList $ floodFill ((x1, y1) `pairPlus` (1,1)) (Set.fromList obstacles))

generateObstacles :: MonadRandom m => Rect -> m [ObstacleTile]
generateObstacles bounds = liftM concat $ replicateM 10 $
  generateObstacle bounds

main :: IO ()
main = bracket_ (hSetEcho stdin False >> hSetBuffering stdin  NoBuffering >> hSetBuffering stdout NoBuffering >> hideCursor) (showCursor >> hSetEcho stdin True) $ do
  let standardViewport = mkRectPosDim (0,0) (80,25)
  obstacles <- evalRandIO $ generateObstacles standardViewport
  exit <- evalRandIO (generateNoConflict standardViewport (map (tPosition . oTile) obstacles))
  setTitle gameTitle
  liftIO clearScreen
  gameLoop [] (initialWorld obstacles [LightSource (10,10) 200] exit standardViewport)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

insideLight :: Coord -> LightSource -> Bool
insideLight (x,y) (LightSource (lx,ly) r) = (x-lx)*(x-lx) + (y-ly)*(y-ly) <= r

viewObstructed :: Map.HashMap Coord Tile -> LightSource -> Coord -> Maybe Coord
viewObstructed obstacles light x = find (`Map.member` obstacles) (line (lsPosition light) x)

tileDiff :: MonadIO m => [Tile] -> [Tile] -> m ()
tileDiff before after = do
  let toClear = before \\ after
      toAdd = after \\ before
  mapM_ clearTile toClear
  mapM_ drawTile toAdd

tileToAssoc :: Tile -> (Coord,Tile)
tileToAssoc t = (tPosition t,t)

renderWorld :: World -> [Tile]
renderWorld w = let obstacleTiles = Map.fromList $ map (tileToAssoc . renderObstacle) (wObstacles w)
                    realTiles = obstacleTiles <> (Map.fromList $ map tileToAssoc [renderHero (wHero w),renderExit (wExit w)])
                    lit = litTiles (wLightSources w)
                    --tilePositions = (Set.fromList . map tPosition) realTiles
                    --obstaclePositions = (Set.fromList . map tPosition) obstacleTiles
                    --litFilter lite = lite `Map.member` realTiles
                    litFilter lite = isNothing (viewObstructed obstacleTiles (head (wLightSources w)) lite)
                    --litFilter = const True
                    renderedLit = (Map.fromList . map (\c -> (c,renderLit c)) . filter litFilter . Set.toList) lit
                    --renderedLit = []
                in (map snd . Map.toList) (realTiles <> renderedLit)

renderLit :: Coord -> Tile
renderLit c = Tile { tCharacter = '.', tSgr = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid White ], tPosition = c }

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
  liftIO $ setSGR [SetConsoleIntensity NormalIntensity,SetColor Foreground Vivid White]
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
    'y' -> return UpLeft
    'u' -> return UpRight
    'b' -> return DownLeft
    'n' -> return DownRight
    _ -> getInput

obstacleAt :: World -> Coord -> Maybe ObstacleTile
obstacleAt w c = find ((== c) . tPosition . oTile) (wObstacles w)

handleDir :: World -> Input -> World
handleDir w input = w { wHero = newCoord }
  where oldCoord@(heroX,heroY) = wHero w
        newCoord' = clampRect (wViewport w) $ case input of
                    Up    -> (heroX, heroY - 1)
                    Down  -> (heroX, heroY + 1)
                    Left  -> (heroX - 1, heroY)
                    Right -> (heroX + 1, heroY)
                    UpLeft    -> (heroX - 1, heroY - 1)
                    UpRight    -> (heroX + 1, heroY - 1)
                    DownLeft  -> (heroX - 1, heroY + 1)
                    DownRight -> (heroX + 1, heroY + 1)
                    _ -> oldCoord
        newCoord = case obstacleAt w newCoord' of
                    Nothing -> newCoord'
                    Just _ -> oldCoord

floodFill :: Coord -> CoordSet -> CoordSet
floodFill start obstacles | start `Set.member` obstacles  = obstacles
                          | otherwise = mconcat $ map step [(0, 1),(1, 0),(-1, 0),(0, -1)]
                          where step c = floodFill (start `pairPlus` c) (Set.insert start obstacles)
