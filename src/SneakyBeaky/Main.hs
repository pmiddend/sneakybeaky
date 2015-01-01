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
import SneakyBeaky.Matrix
import SneakyBeaky.TileTypes
import SneakyBeaky.Generation

type CoordSet = Set.HashSet Coord

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
