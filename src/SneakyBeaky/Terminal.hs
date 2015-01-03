module SneakyBeaky.Terminal(
    run
  , TerminalData
  , TerminalMonad
  , SneakyColor(..)
  , SneakyColorPair
  , mkColorPair
  , Tile(..)
  , render
  , getCharEvent
  , drawStringCentered
  ) where

import           SneakyBeaky.Coord
import           SneakyBeaky.Rect
import qualified UI.NCurses        as C
import Data.List((\\))

type TerminalMonad = C.Curses

data SneakyColor = Red
                 | Green
                 | Blue
                 | White
                 | Transparent
                 deriving(Eq,Show)

data SneakyColorPair = SneakyColorPair {
    cpForeground :: SneakyColor
  , cpBackground :: SneakyColor
  } deriving(Eq,Show)

mkColorPair :: SneakyColor -> SneakyColor -> SneakyColorPair
mkColorPair = SneakyColorPair

data Tile = Tile {
    tPosition  :: !Coord
  , tCharacter :: !Char
  , tColor     :: SneakyColorPair
  } deriving(Eq)

data TerminalData = TerminalData {
    rdWindow    :: C.Window
  , rdViewport  :: Rect
  , rdPrevTiles :: [Tile]
  }

moveCursor :: Coord -> C.Update ()
moveCursor (x,y) = C.moveCursor (fromIntegral y) (fromIntegral x)

drawTile :: Tile -> C.Update ()
drawTile t = do
  moveCursor (tPosition t)
  C.drawString [tCharacter t]

clearTile :: Tile -> C.Update ()
clearTile t = do
  moveCursor (tPosition t)
  C.drawString " "

tileDiff :: [Tile] -> [Tile] -> C.Update ()
tileDiff before after = do
  let toClear = before \\ after
      toAdd = after \\ before
  mapM_ clearTile toClear
  mapM_ drawTile toAdd

render :: TerminalData -> [Tile] -> TerminalMonad TerminalData
render rd ts = do
  C.updateWindow (rdWindow rd) $ tileDiff (rdPrevTiles rd) ts
  C.render
  return (rd {rdPrevTiles = ts})

getCharEvent :: TerminalData -> TerminalMonad Char
getCharEvent rd = do
  e <- C.getEvent (rdWindow rd) Nothing
  case e of
    Just (C.EventCharacter c) -> return c
    _ -> getCharEvent rd

clearScreen :: [Tile] -> C.Update ()
clearScreen = mapM_ clearTile

drawStringCentered :: TerminalData -> String -> TerminalMonad ()
drawStringCentered rd s = C.updateWindow (rdWindow rd) (clearScreen (rdPrevTiles rd) >> moveCursor (((\x -> x - length s).(`div`2). fst . rDim . rdViewport) rd,((`div`2). snd . rDim . rdViewport) rd) >> C.drawString s) >> C.render

run :: Rect -> (TerminalData -> TerminalMonad a) -> IO a
run standardViewport a = C.runCurses $ do
  C.setEcho False
  w <- C.newWindow ((fromIntegral . snd . rDim) standardViewport) ((fromIntegral . fst . rDim) standardViewport) 0 0
  _ <- C.setCursorMode C.CursorInvisible
  a (TerminalData w standardViewport [])

