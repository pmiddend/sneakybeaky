module SneakyBeaky.Terminal where

import Control.Monad.IO.Class(MonadIO,liftIO)
import qualified System.Console.ANSI as AnsiTerm
import Prelude(String,Char,($),(.))
import qualified System.IO as SystemIO
import SneakyBeaky.Coord

clearScreen :: MonadIO m => m ()
clearScreen = liftIO AnsiTerm.clearScreen

putStrLn :: MonadIO m => String -> m ()
putStrLn s = liftIO (SystemIO.putStrLn s)

putStr :: MonadIO m => String -> m ()
putStr s = liftIO (SystemIO.putStr s)

setCursorPosition :: MonadIO m => Coord -> m ()
setCursorPosition (x,y) = liftIO $ AnsiTerm.setCursorPosition y x

setSGR :: MonadIO m => [AnsiTerm.SGR] -> m ()
setSGR = liftIO . AnsiTerm.setSGR

getChar :: MonadIO m => m Char
getChar = liftIO SystemIO.getChar

hideCursor :: MonadIO m => m ()
hideCursor = liftIO AnsiTerm.hideCursor

showCursor :: MonadIO m => m ()
showCursor = liftIO AnsiTerm.showCursor

setTitle :: MonadIO m => String -> m ()
setTitle = liftIO . AnsiTerm.setTitle
