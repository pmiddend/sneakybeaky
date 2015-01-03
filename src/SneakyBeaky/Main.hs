module Main where

import           Control.Applicative    ((<$>), (<|>))
import Control.Monad(replicateM)
import qualified Data.HashMap.Strict    as Map
import           Data.List              (find)
import           Data.Maybe             (isJust, isNothing)
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import           Prelude                hiding (Either (..), getChar, putStr,
                                         putStrLn)
import           SneakyBeaky.Coord
import           SneakyBeaky.Generation
import           SneakyBeaky.Lifted
import           SneakyBeaky.List
import           SneakyBeaky.Rect
import           SneakyBeaky.Terminal
import           SneakyBeaky.TileTypes

data World = World {
    wHero         :: !Coord
  , wObstacles    :: ![ObstacleTile]
  , wEnemies      :: ![Enemy]
  , wExit         :: !Coord
  , wLightSources :: ![LightSource]
  , wViewport     :: !Rect
  }

data Input = Up
           | Down
           | Left
           | Right
           | Stand
           | Exit
           | UpLeft
           | UpRight
           | DownLeft
           | DownRight
           deriving (Eq)

lightTiles :: LightSource -> CoordSet
lightTiles ls = let (cx,cy) = lsPosition ls
                    r = lsRadius ls
                    ts = [(x,y) | x <- [cx-r..cx+r],y <- [cy-r..cy+r]]
                in Set.fromList $ filter (\(x,y) -> (x-cx)*(x-cx) + (cy-y)*(cy-y) <= r) ts

lightTilesUnion :: [LightSource] -> CoordSet
lightTilesUnion = Set.unions . map lightTiles

gameTitle :: String
gameTitle = "sneakybeaky"

initialWorld :: [ObstacleTile] -> [Enemy] -> [LightSource] -> Coord -> Rect -> World
initialWorld obstacles enemies lightSources exit viewport = World {
    wHero = (0,0)
  , wObstacles = obstacles
  , wExit = exit
  , wLightSources = lightSources
  , wViewport = viewport
  , wEnemies = enemies
  }

main :: IO ()
main = do
  let viewport = mkRectPosDim (0,0) (80,25)
  run viewport $ do
      obstacles <- evalRandIO $ generateObstacles viewport
      let obstaclePositions = Set.fromList ((tPosition . oTile) <$> obstacles)
      enemies <- evalRandIO $ replicateM 20 (generateEnemy viewport obstaclePositions)
      lightSources <- evalRandIO (replicateM 12 (generateLightSource viewport obstaclePositions))
      (start,exit) <- evalRandIO (generateStartAndExit viewport obstaclePositions)
      let world = World {
              wHero = start
            , wObstacles = obstacles
            , wExit = exit
            , wLightSources = lightSources
            , wViewport = viewport
            , wEnemies = enemies
            }
      gameLoop world

insideLight :: Coord -> LightSource -> Bool
insideLight (x,y) (LightSource (lx,ly) r) = (x-lx)*(x-lx) + (y-ly)*(y-ly) <= r

viewObstructed :: CoordSet -> Coord -> Coord -> Maybe Coord
viewObstructed obstacles from to = find (`Set.member` obstacles) (line from to)

tileToAssoc :: Tile -> (Coord,Tile)
tileToAssoc t = (tPosition t,t)

obstacleTilesAsMap :: World -> Map.HashMap Coord Tile
obstacleTilesAsMap w = Map.fromList $ map (tileToAssoc . renderObstacle) (wObstacles w)

obstaclesAsSet :: World -> CoordSet
obstaclesAsSet w = Set.fromList $ map (tPosition . oTile) (wObstacles w)

litTiles :: World -> CoordSet
litTiles w = let lights = wLightSources w
                 lit = lightTilesUnion lights
                 obstacleTiles = obstaclesAsSet w
                 litFilter lite = any isNothing (map (\l -> viewObstructed obstacleTiles (lsPosition l) lite) lights)
             in Set.filter litFilter lit

renderWorld :: World -> Rect -> [Tile]
renderWorld w viewport =
  let obstacleTiles = obstacleTilesAsMap w
      enemyTiles = Map.fromList $ map tileToAssoc $ concatMap renderEnemy (wEnemies w)
      realTiles = obstacleTiles <> enemyTiles <> Map.fromList (map tileToAssoc [renderHero (wHero w),renderExit (wExit w)])
      renderedLit = (Map.fromList . map (\c -> (c,renderLit c)) . Set.toList) (litTiles w)
  in (filter ((\p -> insideRect viewport p && p /= ((\(x,y) -> (x-1,y-1)) (rBottomRight viewport))) . tPosition) . Map.elems) (realTiles <> renderedLit)

renderLit :: Coord -> Tile
renderLit c = Tile { tCharacter = '.', tPosition = c,tColor = mkColorPair Blue Transparent }

renderEnemy :: Enemy -> [Tile]
renderEnemy e | eVisible e = [eTile e]
              | otherwise = []

renderHero :: Coord -> Tile
-- renderHero c = Tile { tCharacter = '@', tSgr = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ], tPosition = c }
renderHero c = Tile { tCharacter = '@', tPosition = c,tColor = mkColorPair White Transparent }

renderExit :: Coord -> Tile
renderExit c = Tile {
    tCharacter = '>'
  , tPosition = c
  , tColor = mkColorPair Blue Transparent
  }

renderObstacle :: ObstacleTile -> Tile
renderObstacle = oTile

showMessageAndWait :: String -> TerminalMonad ()
showMessageAndWait s = drawStringCentered s >> getCharEvent >> return ()

gameLoop :: World -> TerminalMonad ()
gameLoop w =
  if wHero w == wExit w
  then showMessageAndWait "You won!"
  else do
    render (renderWorld (updateEnemyVisibility w) (wViewport w))
    input <- getInput
    case input of
      Exit -> return ()
      _    ->
        case updateEnemies w of
          Nothing -> showMessageAndWait "Game over!"
          Just newWorld -> do
            let inputResult = handleDir newWorld input
            if isJust (enemyAt inputResult (wHero inputResult))
              then showMessageAndWait "Game over!"
              else gameLoop inputResult

getInput :: TerminalMonad Input
getInput = do
  char <- getCharEvent
  case char of
    'q' -> return Exit
    'k' -> return Up
    'j' -> return Down
    'h' -> return Left
    '.' -> return Stand
    'l' -> return Right
    'y' -> return UpLeft
    'u' -> return UpRight
    'b' -> return DownLeft
    'n' -> return DownRight
    _ -> getInput

obstacleAt :: World -> Coord -> Maybe ObstacleTile
obstacleAt w c = find ((== c) . tPosition . oTile) (wObstacles w)

enemyAt :: World -> Coord -> Maybe Enemy
enemyAt w c = find ((== c) . tPosition . eTile) (wEnemies w)

playerAt :: World -> Coord -> Bool
playerAt w c = wHero w == c

replaceEnemy :: World -> Enemy -> Enemy -> World
replaceEnemy w old new = w { wEnemies = replaceBy (wEnemies w) ((== (tPosition . eTile) old). tPosition . eTile) new }

updateEnemyVisibility :: World -> World
updateEnemyVisibility w = let os = obstaclesAsSet w
                          in foldr (updateEnemyVisibility' os) w (wEnemies w)
  where updateEnemyVisibility' os e w' = replaceEnemy w' e (e { eVisible = isNothing (viewObstructed os (wHero w') (tPosition . eTile $ e))})

updateEnemies :: World -> Maybe World
updateEnemies ow = let lt = litTiles ow
                       foldEnemy' e w' = w' >>= \w -> let re = updateEnemy w lt e
                                                      in if (tPosition . eTile) re == wHero ow
                                                         then Nothing
                                                         else Just (replaceEnemy w e re)
                   in foldr foldEnemy' (Just ow) (wEnemies ow)

maxFramesSeen :: Int
maxFramesSeen = 3

updateEnemy :: World -> CoordSet -> Enemy -> Enemy
updateEnemy w lt e | eAggro e = updateEnemyAggro w lt e
                   | otherwise = updateEnemyCalm w lt e

updateEnemyAggro :: World -> CoordSet -> Enemy -> Enemy
updateEnemyAggro w _ e = case calculateOptimalPath (obstaclesAsSet w) ((tPosition . eTile) e) (wHero w) of
  Nothing -> e
  Just [] -> e
  Just (x:_) -> e { eTile = (eTile e) { tPosition = x } }

updateEnemyCalm :: World -> CoordSet -> Enemy -> Enemy
updateEnemyCalm w lt e =
  let oldPosition = tPosition (eTile e)
      newPosition' = oldPosition `pairPlus` eWalkingDir e
      isAtTurningPoint = eCurrentWalk e + 1 == eWalkingRadius e
      isObstructed = isJust (obstacleAt w newPosition') || isJust (enemyAt w newPosition')
      isTurning = isAtTurningPoint || isObstructed
      newWalkingDir = if isTurning
                      then pairNegate (eWalkingDir e)
                      else eWalkingDir e
      newPosition = if isObstructed then oldPosition else newPosition'
      newVisible = isNothing (viewObstructed (obstaclesAsSet w) (wHero w) newPosition)
      newFramesSeen = eFramesSeen e + if wHero w `Set.member` lt && newVisible then 1 else 0
      --newCharacter = tCharacter (eTile e)
      newAggro = newFramesSeen >= maxFramesSeen
      newColor = if newAggro
                 then Red
                 else case newFramesSeen of
                       0 -> White
                       1 -> Cyan
                       2 -> Yellow
                       _ -> Green
      newTile = (eTile e) { tPosition = newPosition,tColor = mkColorPair newColor Transparent }
      newCurrentWalk = if isAtTurningPoint
                       then 0
                       else eCurrentWalk e + 1
  in Enemy newTile newAggro newWalkingDir (eWalkingRadius e) newCurrentWalk newFramesSeen newVisible

handleDir :: World -> Input -> World
handleDir w input = w { wHero = newCoord }
  where oldCoord@(heroX,heroY) = wHero w
        newCoord' = case input of
                    Up    -> (heroX, heroY - 1)
                    Down  -> (heroX, heroY + 1)
                    Left  -> (heroX - 1, heroY)
                    Right -> (heroX + 1, heroY)
                    UpLeft    -> (heroX - 1, heroY - 1)
                    UpRight    -> (heroX + 1, heroY - 1)
                    DownLeft  -> (heroX - 1, heroY + 1)
                    DownRight -> (heroX + 1, heroY + 1)
                    _ -> oldCoord
        newCoord = if insideRect (wViewport w) newCoord' then case obstacleAt w newCoord' of
                     Nothing -> newCoord'
                     Just _ -> oldCoord
                   else oldCoord
