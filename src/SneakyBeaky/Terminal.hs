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

import           Control.Monad.State.Strict (StateT,evalStateT,get,put)
import           Control.Monad.Trans.Class (lift)
import           Data.List                  ((\\))
import           SneakyBeaky.Coord
import           SneakyBeaky.Rect
import qualified UI.NCurses                 as C

type TerminalMonad = StateT TerminalData C.Curses

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

render :: [Tile] -> TerminalMonad ()
render ts = do
  rd <- get
  lift $ C.updateWindow (rdWindow rd) $ tileDiff (rdPrevTiles rd) ts
  lift C.render
  put (rd {rdPrevTiles = ts})

getCharEvent :: TerminalMonad Char
getCharEvent = do
  rd <- get
  e <- lift $ C.getEvent (rdWindow rd) Nothing
  case e of
    Just (C.EventCharacter c) -> return c
    _ -> getCharEvent

clearScreen :: [Tile] -> C.Update ()
clearScreen = mapM_ clearTile

drawStringCentered :: String -> TerminalMonad ()
drawStringCentered s = do
  rd <- get
  lift $ C.updateWindow (rdWindow rd) (clearScreen (rdPrevTiles rd) >> moveCursor (((\x -> x - length s).(`div`2). fst . rDim . rdViewport) rd,((`div`2). snd . rDim . rdViewport) rd) >> C.drawString s) >> C.render

run :: Rect -> TerminalMonad a -> IO a
run standardViewport a = C.runCurses $ do
  C.setEcho False
  w <- C.newWindow ((fromIntegral . snd . rDim) standardViewport) ((fromIntegral . fst . rDim) standardViewport) 0 0
  _ <- C.setCursorMode C.CursorInvisible
  evalStateT a (TerminalData w standardViewport [])

