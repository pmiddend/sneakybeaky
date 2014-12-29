module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Exception.Base(bracket_)
import System.Random
import Control.Monad.Random
import Data.List(find)

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
}

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq)

gameTitle :: String
gameTitle = "sneakybeaky"

initialWorld :: World
initialWorld = World (0,0) []

generateObstacles :: RandomGen g => Rand g [ObstacleTile]
generateObstacles = return [ObstacleTile (Tile {tCharacter = '|', tSgr = [], tPosition = (1,1)}) True]

main :: IO ()
main = bracket_ (hSetEcho stdin False >> hSetBuffering stdin  NoBuffering >> hSetBuffering stdout NoBuffering >> hideCursor) (showCursor >> hSetEcho stdin True) $ do
  obstacles <- evalRandIO generateObstacles
  setTitle gameTitle
  gameLoop (World (0,0) obstacles)

renderWorld :: World -> [Tile]
renderWorld w = [renderHero (wHero w)] ++ map renderObstacle (wObstacles w)

renderHero :: Coord -> Tile
renderHero c = Tile { tCharacter = '@', tSgr = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ], tPosition = c }

renderObstacle :: ObstacleTile -> Tile
renderObstacle = oTile

gameLoop :: MonadIO m => World -> m ()
gameLoop w = do
  liftIO clearScreen
  mapM_ drawTile (renderWorld w)
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
drawHero c = drawTile $ Tile { tCharacter = '@', tSgr = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ], tPosition = c}

-- receive a character and return our Input data structure,
-- recursing on invalid input
getInput :: MonadIO m => m Input
getInput = do
  char <- liftIO getChar
  case char of
    'q' -> return Exit
    'w' -> return Up
    's' -> return Down
    'a' -> return Left
    'd' -> return Right
    _ -> getInput

obstacleAt :: World -> Coord -> Maybe ObstacleTile
obstacleAt w c = find ((== c) . tPosition . oTile) (wObstacles w)

-- given a world and a direction, 'adjust' the hero's position, and loop
-- with our updated hero
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
