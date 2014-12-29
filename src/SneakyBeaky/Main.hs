module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Monad.IO.Class(MonadIO,liftIO)
import Control.Exception.Base(bracket_)

type Coord = (Int, Int)

data World = World {
  wHero :: Coord
}

data Input = Up
           | Down
           | Left
           | Right
           | Exit
           deriving (Eq)

gameTitle :: String
gameTitle = "Thieflike"

main :: IO ()
main = bracket_ (hSetEcho stdin False >> hSetBuffering stdin  NoBuffering >> hSetBuffering stdout NoBuffering >> hideCursor) (showCursor >> hSetEcho stdin True) $ do
  setTitle gameTitle
  gameLoop $ World (0, 0)

gameLoop :: MonadIO m => World -> m ()
gameLoop world@(World hero) = do
  liftIO clearScreen
  drawHero hero
  input <- getInput
  case input of
    Exit -> return ()
    _    -> handleDir world input

drawHero :: MonadIO m => (Int,Int) -> m ()
drawHero (heroX, heroY) = do
  liftIO $ setCursorPosition heroY heroX
  liftIO $ setSGR [ SetConsoleIntensity BoldIntensity
                  , SetColor Foreground Vivid Blue ]
  liftIO $ putStr "@"

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

-- given a world and a direction, 'adjust' the hero's position, and loop
-- with our updated hero
handleDir :: MonadIO m => World -> Input -> m ()
handleDir w@(World (heroX, heroY)) input = gameLoop $ w { wHero = newCoord }
  where newCoord = case input of
                    Up    -> (heroX, heroY - 1)
                    Down  -> (heroX, heroY + 1)
                    Left  -> (heroX - 1, heroY)
                    Right -> (heroX + 1, heroY)
