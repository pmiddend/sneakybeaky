module SneakyBeaky.TileTypes where

import           SneakyBeaky.Coord
import SneakyBeaky.Terminal

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
