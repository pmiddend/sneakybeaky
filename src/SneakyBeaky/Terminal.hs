module SneakyBeaky.Terminal where

import qualified UI.NCurses             as C
import SneakyBeaky.Rect

type TerminalMonad = C.Curses

data TerminalData = TerminalData {
    rdWindow :: C.Window
  , rdViewport :: Rect
  }

run :: Rect -> (TerminalData -> TerminalMonad a) -> IO a
run standardViewport a = C.runCurses $ do
  C.setEcho False
  w <- C.newWindow ((fromIntegral . snd . rDim) standardViewport) ((fromIntegral . fst . rDim) standardViewport) 0 0
  _ <- C.setCursorMode C.CursorInvisible
  a (TerminalData w standardViewport)

