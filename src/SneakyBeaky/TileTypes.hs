module SneakyBeaky.TileTypes where

import           SneakyBeaky.Coord

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

data Tile = Tile {
    tPosition  :: !Coord
  , tCharacter :: !Char
  , tColor     :: SneakyColorPair
  } deriving(Eq)

data ObstacleTile = ObstacleTile {
    oTile  :: !Tile
  , oSolid :: !Bool
  }

data LightSource = LightSource {
    lsPosition :: !Coord
  , lsRadius   :: !Int
  }

data Enemy = Enemy {
    eTile          :: !Tile
  , eAggro         :: !Bool
  , eWalkingDir    :: !Coord
  , eWalkingRadius :: !Int
  , eCurrentWalk   :: !Int
  , eFramesSeen    :: !Int
  , eVisible       :: !Bool
  }
