{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import           ClassyPrelude
import Control.Monad.Writer(Writer,tell)
import qualified Data.List.NonEmpty as NE
import           Control.Lens                            (makeLenses,lens,Lens',_2,_1,
                                                          makePrisms, to, (^.),view,(&),ix,(.~))
import           Linear.V2
import           Prelude                                 ()
import           SneakyBeaky.GenerateCircle
import           System.Console.SneakyTerm.Color
import           System.Console.SneakyTerm.ColorPair
import           System.Console.SneakyTerm.MonadTerminal
import           System.Console.SneakyTerm.PointInt
import           System.Console.SneakyTerm.Rect
import           System.Console.SneakyTerm.Tile

divNearest :: Integral a => a -> a -> a
divNearest x y = (x + (y `div` 2)) `div` y

maybeFlipped :: Maybe a -> b -> (a -> b) -> b
maybeFlipped Nothing b _ = b
maybeFlipped (Just x) _ f = f x

{-
  Testing simple case:

  castShadow (x=1,y=1,ss=1,se=0) =
    endx = 0
    (visibleTiles,remainder) = break isBlocked [V2 [1..0] 1]
    visibleTiles = [V2 1 1,V2 0 1]
    remainder = []
    result = visibleTiles <> castShadow (x=2,y=2,ss=1,se=0)

  castShadow (x=2,y=2,ss=1,se=0) =
    endx = 0
    (visibleTiles,remainder) = break isBlocked [V2 [2..0] 2]

  ...

  works

  Testing example case:

  castShadow (x=12,y=12,ss=1,se=0) =
    endx = 0
    (visibleTiles,remainder) = break isBlocked [V2 [12..0] 12]
    visibleTiles = [V2 12 12,V2 11 12,V2 10 12]
    remainder = [V2 9 12,V2 8 12,...]
    xs = [V2 8 12,V2 7 12,...]
    newse = fromIntegral 10 / fromIntegral 12 = 0.83
    blockerRecursion =
      castShadow (x=13,y=13,ss=1,se=0.83) =
        endx = 0.83 * 13 = 10.79
        (visibleTiles,remainder) = break isBlocked [V2 [13..11] 13]
        visibleTiles = [V2 13 13,V2 12 13,V2 11 13]
        remainder = []
    nextNonblocked = dropWhile isBlocked [V2 8 12,V2 7 12,...]
    nextNonblocked = [V2 7 12,V2 6 12,...]
    (fnbx=7,fnby=12)
    newss = 8/13 = 0.61
    remainder' =
      castShadow(x=7,y=12,ss=0.61,se=1)
 -}

data Span a = Span Bool Bool (NE.NonEmpty a)
              deriving(Functor)

deriving instance Show a => Show (Span a)

zipLeft :: [a] -> [(Maybe a,a)]
zipLeft xs = zip (Nothing : (Just <$> xs)) xs

zipRight :: [a] -> [(a,Maybe a)]
zipRight xs = zip xs ((Just <$> drop 1 xs) <> [Nothing])

zipWithLeft :: (Maybe b -> b -> c) -> [b] -> [c]
zipWithLeft f xs = zipWith f (Nothing : (Just <$> xs)) xs

zipWithRight :: (a -> Maybe a -> c) -> [a] -> [c]
zipWithRight f xs = zipWith f xs ((Just <$> drop 1 xs) <> [Nothing])

data Neighbors a = Neighbors (Maybe a) a (Maybe a) deriving(Functor)

leftNeighbor :: Lens' (Neighbors a) (Maybe a)
leftNeighbor = lens (\(Neighbors l _ _) -> l) (\(Neighbors _ x r) l -> Neighbors l x r)

rightNeighbor :: Lens' (Neighbors a) (Maybe a)
rightNeighbor = lens (\(Neighbors _ _ r) -> r) (\(Neighbors l x _) r -> Neighbors l x r)

atElem :: Lens' (Neighbors a) a
atElem = lens (\(Neighbors _ x _) -> x) (\(Neighbors l _ r) x -> Neighbors l x r)

withNeighbors :: [b] -> [Neighbors b]
withNeighbors xs = zipWithRight (\(left,x) right -> Neighbors left x (snd <$> right)) (zipWithLeft (,) xs)

-- | Calculate adjacent spans of blocked/free tiles
spans :: forall a.Enum a =>
         (a -> Bool) -- ^ Tile position (reduced to one coordinate) to translucency
         -> a -- ^ x coordinate start
         -> a -- ^ x coordinate end
         -> [Span a]
spans isBlocked xs xe =
  let
    blocks :: [a]
    blocks = reverse [xe..xs]
    groups :: [Neighbors (Bool,NE.NonEmpty a)]
    groups = withNeighbors (groupByEquals isBlocked blocks)
    relevantBlocks :: [Neighbors (Bool,NE.NonEmpty a)]
    relevantBlocks = filter (not . view (atElem . _1)) groups
    mapBlock :: Neighbors (Bool,NE.NonEmpty a) -> Span a
    mapBlock block = Span (block ^. leftNeighbor . to isJust) (block ^. rightNeighbor . to isJust) (block ^. atElem . _2)
  in
    mapBlock <$> relevantBlocks

slope :: Fractional a => V2 a -> a
slope (V2 x y) = x / y

groupByEquals :: (Foldable f, Eq b) => (a -> b) -> f a -> [(b, NE.NonEmpty a)]
groupByEquals f xs = fmap (\l -> (f (NE.head l),l)) . NE.groupBy ((==) `on` f) $ xs

{-
 - Second implementation, relying on the spans
 - Calculate spans of free blocks
 - Return each of the free spans
 - For each span, start recursive calculation
 -}
printShadowCast :: [PointInt] -> IO ()
printShadowCast r =
  let
    lines :: [(Int,NE.NonEmpty PointInt)]
    lines = groupByEquals (view _y) (sortBy (comparing (view _y)) r)
    sortedLines :: [(Int,NE.NonEmpty PointInt)]
    sortedLines = sortBy (comparing fst) lines
    numLines = length sortedLines
    mapLine :: (Int,NE.NonEmpty PointInt) -> Text
    mapLine (n,points) = foldr (\(V2 x _) s -> s & ix (numLines - x - 1) .~ '.') (replicate numLines ' ') points
    mappedLines :: [Text]
    mappedLines = mapLine <$> sortedLines
  in
    mapM_ putStrLn (reverse mappedLines)

tagFirst :: [a] -> [(Bool,a)]
tagFirst [] = []
tagFirst (x:xs) = (True,x) : map (False,) xs

packShow :: Show a => a -> Text
packShow = pack . show

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

{- new version
castShadow :: forall a.( Show a,RealFrac a ) =>
  V2 a -- ^ Starting point
  -> a -- ^ Start slope
  -> a -- ^ End slope
  -> (PointInt -> Bool) -- ^ Tile position to translucency
  -> Writer [Text] [PointInt] -- ^ List of lit tiles
castShadow (V2 sx sy) ss se isBlocked =
  let
    -- Integral version of the start vector
    (V2 sxi syi) = round <$> (V2 sx sy)
    -- Non-blocked spans
    spanResults :: Span PointInt
    spanResults = (`V2` syi) <$> spans (isBlocked . (`V2` syi)) sxi (round (se * sx))
    recursion :: Span PointInt -> Writer [Text] [PointInt]
    recursion (isFirst,xs) =
      let
        f = fromIntegral <$> NE.head xs :: V2 a
        fs = slope f
        l = fromIntegral <$> NE.last xs :: V2 a
        ls = if isFirst then slope l else se
      in do
        tell ["recursion (f=" <> packShow isFirst <> ") at " <> packShow (f + V2 fs 1) <> ", ss=" <> packShow fs <> ", se=" <> packShow ls <> ", f=" <> packShow f <> ", l=" <> packShow l]
        result <- castShadow (f + V2 fs 1) fs ls isBlocked
        return $ NE.toList xs <> result
  in do
    tell ["Span result sx=" <> packShow sx <> ", sy=" <> packShow sy <> ": " <> packShow spanResults]
    result <- spanResult spanResults freeRecursion (concatMapM recursion . tagFirst)
    tell ["Returning " <> packShow result]
    return result
-}

{-
-- If starts blocked, calculate new slope, too
castShadow :: forall a.( Show a,RealFrac a ) =>
  V2 a -- ^ Starting point
  -> a -- ^ Start slope
  -> a -- ^ End slope
  -> (PointInt -> Bool) -- ^ Tile position to translucency
  -> Writer [Text] [PointInt] -- ^ List of lit tiles
castShadow (V2 sx sy) ss se isBlocked =
  let
    -- Integral version of the start vector
    (V2 sxi syi) = round <$> (V2 sx sy)
    -- Non-blocked spans
    spanResults :: SpanResult PointInt
    spanResults = (`V2` syi) <$> spans (isBlocked . (`V2` syi)) sxi (round (se * sx))
    freeRecursion :: NE.NonEmpty PointInt -> Writer [Text] [PointInt]
    freeRecursion xs = do
      tell ["Free recursion on " <> packShow xs]
      let
        f = fromIntegral <$> NE.head xs :: V2 a
        fs = slope f
      result <- castShadow (f + V2 fs 1) fs se isBlocked
      return $ NE.toList xs <> result
    recursion :: (Bool,NE.NonEmpty PointInt) -> Writer [Text] [PointInt]
    recursion (isFirst,xs) =
      let
        f = fromIntegral <$> NE.head xs :: V2 a
        fs = slope f
        l = fromIntegral <$> NE.last xs :: V2 a
        ls = if isFirst then slope l else se
      in do
        tell ["recursion (f=" <> packShow isFirst <> ") at " <> packShow (f + V2 fs 1) <> ", ss=" <> packShow fs <> ", se=" <> packShow ls <> ", f=" <> packShow f <> ", l=" <> packShow l]
        result <- castShadow (f + V2 fs 1) fs ls isBlocked
        return $ NE.toList xs <> result
  in do
    tell ["Span result sx=" <> packShow sx <> ", sy=" <> packShow sy <> ": " <> packShow spanResults]
    result <- spanResult spanResults freeRecursion (concatMapM recursion . tagFirst)
    tell ["Returning " <> packShow result]
    return result
castShadow :: forall a.( Show a,RealFrac a ) =>
  V2 a -- ^ Starting point
  -> a -- ^ Start slope
  -> a -- ^ End slope
  -> (PointInt -> Bool) -- ^ Tile position to translucency
  -> Writer [Text] [PointInt] -- ^ List of lit tiles
castShadow (V2 sx sy) ss se isBlocked =
  let
    -- Integral version of the start vector
    (V2 sxi syi) = round <$> (V2 sx sy)
    -- Non-blocked spans
    spanResults :: SpanResult PointInt
    spanResults = (`V2` syi) <$> spans (isBlocked . (`V2` syi)) sxi (round (se * sx))
    freeRecursion :: NE.NonEmpty PointInt -> Writer [Text] [PointInt]
    freeRecursion xs = do
      tell ["Free recursion on " <> packShow xs]
      let
        f = fromIntegral <$> NE.head xs :: V2 a
        fs = slope f
      result <- castShadow (f + V2 fs 1) fs se isBlocked
      return $ NE.toList xs <> result
    recursion :: (Bool,NE.NonEmpty PointInt) -> Writer [Text] [PointInt]
    recursion (isFirst,xs) =
      let
        f = fromIntegral <$> NE.head xs :: V2 a
        fs = slope f
        l = fromIntegral <$> NE.last xs :: V2 a
        ls = if isFirst then slope l else se
      in do
        tell ["recursion (f=" <> packShow isFirst <> ") at " <> packShow (f + V2 fs 1) <> ", ss=" <> packShow fs <> ", se=" <> packShow ls <> ", f=" <> packShow f <> ", l=" <> packShow l]
        result <- castShadow (f + V2 fs 1) fs ls isBlocked
        return $ NE.toList xs <> result
  in do
    tell ["Span result sx=" <> packShow sx <> ", sy=" <> packShow sy <> ": " <> packShow spanResults]
    result <- spanResult spanResults freeRecursion (concatMapM recursion . tagFirst)
    tell ["Returning " <> packShow result]
    return result
-}
  {- First implementation
  let
    endx = se * x
    (visibleTiles,remainder) = break isBlocked [V2 x' (round y) | x' <- [round x..round endx]]
  in
   maybeFlipped (uncons remainder) (visibleTiles <> castShadow (V2 (x + ss) (y+1)) ss se isBlocked) $ \(V2 fbx fby,xs) ->
     let
       newse = fromIntegral (fbx + 1) / fromIntegral fby
       blockerRecursion = castShadow (V2 (x + ss) (y+1)) ss newse isBlocked
       nextNonblocked = dropWhile isBlocked xs
       remainder' =
         maybeFlipped (headMay nextNonblocked) [] $ \(V2 fnbx fnby) ->
           let
             newss = fromIntegral (fnbx + 1) / fromIntegral (fnby + 1)
           in
             castShadow (fromIntegral <$> (V2 fnbx fnby)) newss se isBlocked
     in
       visibleTiles <> blockerRecursion <> remainder'
-}


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
lampLitTiles :: (PointInt -> Translucency) -> -- ^ Tile to translucency
                Lamp ->
                [PointInt]           -- ^ All lit points
lampLitTiles isSolid l = generateCirclePoints ( l ^. lampPosition ) (l ^. lampRadius)-- >>= (line (l ^. lampPosition))

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
  (litTile <$> ((g ^. gameLamps) >>= lampLitTiles undefined)) <>
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
