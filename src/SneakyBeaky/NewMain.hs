{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Linear.V2
import Control.Lens(makeLenses,(^.))
import System.Control.SneakyTerm.Tile

type Point = V2 Int

data Translucency = Translucent
                  | Opaque

data Player = Player {
    _playerPosition    :: Point
  , _playerWaterArrows :: Int
  , _playerKillArrows  :: Int
  , _playerKeys        :: [Key]
  }

$(makeLenses ''Player)

data EnemyState = EnemyRelaxed
                | EnemyAlert
                | EnemyPursuing

data Enemy = Enemy {
    _enemyCharacter       :: Char
  , _enemyRunPath         :: [Point]
  , _enemyLastReachedNode :: Int
  , _enemyState           :: EnemyState
  , _enemyPosition        :: Point
  }

data Solid = Solid {
    _solidCharacter   :: Char
  , _solidPosition    :: Point
  , _solidMoveable    :: Bool
  , _solidTranslucency :: Translucency
  , _solidSeen        :: Bool
  }

data Exit = Exit {
    _exitPosition :: Point
  , _exitSeen     :: Bool
  }

data Color = ColorRed
           | ColorGreen
           | ColorBlue

data Key = Key {
    _keyColor :: Color
  , _keySeen  :: Bool
  }

data Lamp = Lamp {
    _lampPosition  :: Point
  , _lampOpenFlame :: Bool
  , _lampRadius    :: Int
  }

data WaterArrow = WaterArrow {
    _waterArrowSeen :: Bool
  }

data KillArrow = KillArrow {
    _killArrowSeen :: Bool
  }

data Game = Game {
    _gamePlayer      :: Player
  , _gameEnemies     :: [Enemy]
  , _gameSolids      :: [Solid]
  , _gameExit        :: Exit
  , _gameKeys        :: [(Point,Key)]
  , _gameLamps       :: [Lamp]
  , _gameWaterArrows :: [(Point,WaterArrow)]
  , _gameKillArrows  :: [(Point,KillArrow)]
  }

{-
Notes on rendering

  - The player can see in all directions at once
  - The player can see...
    - everything lit by a lamp
    - everything with the seen flag
    - himself
    - everything in its sight radius

  - Lamp visibility determination...
    - take all points in the circle boundary around the lamp
    - cast a ray from the lamp to the point, mark every point on the line as lit until a non-translucent solid is encountered.

  - During rendering, lit tiles are marked with a dot (.), unless they're occupied by something else already
  - Player visibility determination...
    - for all lit tiles, cast a ray from the player to the tile, show only if an intersection occurs
    - for all objects inside the player near sight radius, do the lamp visibility determination test, render those, too

  - Lamps do not move and certain solids don't, too, so some of the visibility work can be done statically?
  - What data structures are returned?
-}

-- | Return all points lit by the lamp
lampLitTiles :: Lamp                 -- ^ Lamp to test 
             -> (PointInt -> Translucency)   -- ^ Tile to translucency
             -> [PointInt]           -- ^ All lit points
lampLitTiles = error "lampLitTiles not implemented"

visibleTiles :: Game -> [Tile]
visibleTiles = error "visibleTiles not implemented"

initialGame = Game{
  _gamePlayer = Player{
       _playerPosition = V2 10 10
     , _playerWaterArrows = 0
     , _playerKillArrows = 0
     , _playerKeys = []
    },
  _gameEnemies = [],
  _gameSolids = [],
  _gameExit = Exit{_exitPosition = V2 1 1},
  _
              
  }

playerTile :: Player -> Tile
playerTile p = Tile{
      _tilePosition = p ^. playerPosition
    , _tileCharacter = '@'
    , _tileColor = ColorPair 
  }

gameToTiles :: Game -> [Tile]
gameToTiles g = 

main = 
