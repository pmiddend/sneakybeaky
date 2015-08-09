module Main where

import           Linear.V2

type Point = V2 Int

data Player = Player {
    _playerPosition    :: Point
  , _playerWaterArrows :: Int
  , _playerKillArrows  :: Int
  , _playerKeys        :: [Key]
  }

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
  , _solidTranslucent :: Bool
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

