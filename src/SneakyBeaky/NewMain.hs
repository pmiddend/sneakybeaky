{-# LANGUAGE TemplateHaskell #-}
module Main where

import           ClassyPrelude
import           Control.Lens                            (makeLenses,
                                                          makePrisms, to, (^.))
import           Linear.V2
import           Prelude                                 ()
import           System.Console.SneakyTerm.Color
import           System.Console.SneakyTerm.ColorPair
import           System.Console.SneakyTerm.MonadTerminal
import           System.Console.SneakyTerm.PointInt
import           System.Console.SneakyTerm.Rect
import           System.Console.SneakyTerm.Tile
import SneakyBeaky.GenerateLine
import SneakyBeaky.GenerateCircle

data KeyColor = KeyRed
              | KeyGreen
              | KeyBlue

$(makePrisms ''KeyColor)

data Key = Key {
    _keyColor :: KeyColor
  , _keySeen  :: Bool
  }

$(makeLenses ''Key)

data Translucency = Translucent
                  | Opaque

$(makePrisms ''Translucency)

data Player = Player {
    _playerPosition    :: PointInt
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
  , _enemyRunPath         :: [PointInt]
  , _enemyLastReachedNode :: Int
  , _enemyState           :: EnemyState
  , _enemyPosition        :: PointInt
  }

$(makeLenses ''Enemy)

data Solid = Solid {
    _solidCharacter    :: Char
  , _solidPosition     :: PointInt
  , _solidMoveable     :: Bool
  , _solidTranslucency :: Translucency
  , _solidSeen         :: Bool
  }

$(makeLenses ''Solid)

data Exit = Exit {
    _exitPosition :: PointInt
  , _exitSeen     :: Bool
  }

$(makeLenses ''Exit)

data Lamp = Lamp {
    _lampPosition  :: PointInt
  , _lampOpenFlame :: Bool
  , _lampRadius    :: Int
  }

$(makeLenses ''Lamp)

data WaterArrow = WaterArrow {
    _waterArrowSeen :: Bool
  }

$(makeLenses ''WaterArrow)

data KillArrow = KillArrow {
    _killArrowSeen :: Bool
  }

$(makeLenses ''KillArrow)

data Game = Game {
    _gamePlayer      :: Player
  , _gameEnemies     :: [Enemy]
  , _gameSolids      :: [Solid]
  , _gameExit        :: Exit
  , _gameKeys        :: [(Key,PointInt)]
  , _gameLamps       :: [Lamp]
  , _gameWaterArrows :: [(WaterArrow,PointInt)]
  , _gameKillArrows  :: [(KillArrow,PointInt)]
  }

$(makeLenses ''Game)

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
lampLitTiles l isSolid = generateCirclePoints ( l ^. lampPosition ) (l ^. lampRadius)

litTile :: PointInt -> Tile
litTile p = Tile{
    _tileCharacter = '▒'
  , _tilePosition = p
  , _tileColor = ColorPair Yellow Transparent
  }

visibleTiles :: Game -> [Tile]
visibleTiles = error "visibleTiles not implemented"

initialGame :: Game
initialGame = Game{
  _gamePlayer = Player{
       _playerPosition = V2 10 10
     , _playerWaterArrows = 0
     , _playerKillArrows = 0
     , _playerKeys = []
    },
  _gameEnemies = [Enemy {
                        _enemyCharacter = 'g'
                      , _enemyPosition = V2 2 2}],
  _gameSolids = generateClosedRoom (rectFromOriginAndDim (V2 1 1) (V2 30 10)),
  _gameExit = Exit{_exitPosition = V2 1 1},
  _gameKeys = [(Key{_keyColor = KeyRed},V2 4 4)],
  _gameLamps = [Lamp{_lampPosition = V2 5 5,_lampOpenFlame = False,_lampRadius = 5}],
  _gameWaterArrows = [(WaterArrow{},V2 6 6)],
  _gameKillArrows = [(KillArrow{},V2 7 7)]
  }

playerTile :: Player -> Tile
playerTile p = Tile{
      _tilePosition = p ^. playerPosition
    , _tileCharacter = '@'
    , _tileColor = ColorPair White Transparent
  }

enemyTile :: Enemy -> Tile
enemyTile e = Tile{
    _tilePosition = e ^. enemyPosition
  , _tileCharacter = e ^. enemyCharacter
  , _tileColor = ColorPair White Transparent
  }

solidTile :: Solid -> Tile
solidTile s = Tile{
    _tilePosition = s ^. solidPosition
  , _tileCharacter = s ^. solidCharacter
  , _tileColor = ColorPair White Transparent
  }

exitTile :: Exit -> Tile
exitTile e = Tile{
    _tilePosition = e ^. exitPosition
  , _tileCharacter = '>'
  , _tileColor = ColorPair White Transparent
  }

keyTile :: Key -> PointInt -> Tile
keyTile k p = Tile{
    _tilePosition = p
  , _tileCharacter = '&'
  , _tileColor = ColorPair (keyColorToTermColor (k ^. keyColor)) Transparent
  }
  where keyColorToTermColor KeyRed = Red
        keyColorToTermColor KeyGreen = Green
        keyColorToTermColor KeyBlue = Blue

lampTile :: Lamp -> Tile
lampTile l = Tile{
    _tilePosition = l ^. lampPosition
  , _tileCharacter = if l ^. lampOpenFlame then '"' else '%'
  , _tileColor = ColorPair Yellow Transparent
  }

waterArrowTile :: WaterArrow -> PointInt -> Tile
waterArrowTile w p = Tile{
    _tilePosition = p
  , _tileCharacter = '!'
  , _tileColor = ColorPair Blue Transparent
  }

killArrowTile :: KillArrow -> PointInt -> Tile
killArrowTile k p = Tile{
    _tilePosition = p
  , _tileCharacter = '!'
  , _tileColor = ColorPair Red Transparent
  }

gameToTiles :: Game -> [Tile]
gameToTiles g =
  (litTile <$> ((g ^. gameLamps) >>= (\l -> lampLitTiles l undefined))) <>
  [g ^. gamePlayer . to playerTile,g ^. gameExit . to exitTile] <>
  (enemyTile <$> g ^. gameEnemies) <>
  (solidTile <$> g ^. gameSolids) <>
  (lampTile <$> g ^. gameLamps) <>
  (uncurry keyTile <$> g ^. gameKeys) <>
  (uncurry waterArrowTile <$> g ^. gameWaterArrows) <>
  (uncurry killArrowTile <$> g ^. gameKillArrows)

generateClosedRoom :: RectInt -> [Solid]
generateClosedRoom r = top <> leftTop <> bottom <> leftBottom <> rightBottom <> left <> right <> rightTop
  where
    top = solid '─' <$> [V2 x (r ^. rectTop) | x <- [r ^. rectLeft + 1..r ^. rectRight - 1]]
    bottom = solid '─' <$> [V2 x (r ^. rectBottom) | x <- [r ^. rectLeft + 1..r ^. rectRight - 1]]
    left = solid '│' <$> [V2 (r ^. rectLeft) y | y <- [r ^. rectTop + 1..r ^. rectBottom - 1]]
    right = solid '│' <$> [V2 (r ^. rectRight) y | y <- [r ^. rectTop + 1..r ^. rectBottom - 1]]
    leftTop = return $ solid '┌' (r ^. rectLeftTop)
    rightTop = return $ solid '┐' (r ^. rectRightTop)
    leftBottom = return $ solid '└' (r ^. rectLeftBottom)
    rightBottom = return $ solid '┘' (r ^. rectRightBottom)
    solid c p =
      Solid{
          _solidCharacter = c
        , _solidPosition = p
        , _solidMoveable = False
        , _solidTranslucency = Opaque
        , _solidSeen = False
        }

main :: IO ()
main = runTerminal (rectFromOriginAndDim (V2 0 0) (V2 80 25)) $ do
  tmRender (gameToTiles initialGame)
  _ <- tmCharEvent
  return ()
