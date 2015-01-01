module SneakyBeaky.TileTypes where

import SneakyBeaky.Coord
import System.Console.ANSI

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
