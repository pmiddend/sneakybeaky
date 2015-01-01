module Main where

import Prelude hiding (Either(..),putStr,putStrLn,getChar)
import Control.Applicative((<|>),(<$>))
import System.Console.ANSI(SGR(..),Color(..),ColorIntensity(..),ConsoleLayer(..),ConsoleIntensity(..))
import System.IO hiding (putStr,putStrLn,getChar)
import Control.Monad.IO.Class(MonadIO)
import Control.Exception.Base(bracket_)
import Data.Monoid((<>))
import Data.Maybe(isJust,isNothing)
import Control.Monad.Random
import Data.List(find,(\\))
import qualified Data.HashSet as Set
--import qualified Data.Set as Set
import qualified Data.HashMap.Strict as Map
import SneakyBeaky.Coord
import SneakyBeaky.Rect
import SneakyBeaky.TileTypes
import SneakyBeaky.List
import SneakyBeaky.Generation
import SneakyBeaky.Terminal

data World = World {
    wHero :: !Coord
  , wObstacles :: ![ObstacleTile]
  , wEnemies :: ![Enemy]
  , wExit :: !Coord
  , wLightSources :: ![LightSource]
  , wViewport :: !Rect
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

initialWorld :: [ObstacleTile] -> [LightSource] -> Coord -> Rect -> World
initialWorld obstacles lightSources exit viewport = World {
    wHero = (0,0)
  , wObstacles = obstacles
  , wExit = exit
  , wLightSources = lightSources
  , wViewport = viewport
  , wEnemies = [Enemy (Tile (5,5) '0' []) False (1,0) 10 0 0 False]
  }

initConsole :: IO ()
initConsole = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hideCursor
  setTitle gameTitle

deinitConsole :: IO ()
deinitConsole = do
  showCursor
  hSetEcho stdin True

main :: IO ()
main = bracket_ initConsole deinitConsole $ do
  let standardViewport = mkRectPosDim (0,0) (80,25)
  obstacles <- evalRandIO $ generateObstacles standardViewport
  let obstaclePositions = (tPosition . oTile) <$> obstacles
  exit <- evalRandIO $ generateNoConflict standardViewport obstaclePositions
  lightSourcePosition <- evalRandIO $ generateNoConflict standardViewport obstaclePositions
  clearScreen
  gameLoop [] (initialWorld obstacles [LightSource lightSourcePosition 200] exit standardViewport)
  _ <- getChar
  return ()

insideLight :: Coord -> LightSource -> Bool
insideLight (x,y) (LightSource (lx,ly) r) = (x-lx)*(x-lx) + (y-ly)*(y-ly) <= r

viewObstructed :: CoordSet -> Coord -> Coord -> Maybe Coord
viewObstructed obstacles from to = find (`Set.member` obstacles) (line from to)

tileDiff :: MonadIO m => [Tile] -> [Tile] -> m ()
tileDiff before after = do
  let toClear = before \\ after
      toAdd = after \\ before
  mapM_ clearTile toClear
  mapM_ drawTile toAdd

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
                 litFilter lite = isNothing (foldr ((<|>) . (\l -> viewObstructed obstacleTiles (lsPosition l) lite)) Nothing lights)
             in Set.filter litFilter lit

renderWorld :: World -> [Tile]
renderWorld w = let obstacleTiles = obstacleTilesAsMap w
                    enemyTiles = Map.fromList $ map tileToAssoc $ concatMap renderEnemy (wEnemies w)
                    realTiles = obstacleTiles <> enemyTiles <> Map.fromList (map tileToAssoc [renderHero (wHero w),renderExit (wExit w)])
                    renderedLit = (Map.fromList . map (\c -> (c,renderLit c)) . Set.toList) (litTiles w)
                in (filter ((\(x,y) -> x >= 0 && y >= 0) . tPosition) . map snd . Map.toList) (realTiles <> renderedLit)

renderLit :: Coord -> Tile
renderLit c = Tile { tCharacter = '\x2591', tSgr = [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Blue ], tPosition = c }

renderEnemy :: Enemy -> [Tile]
renderEnemy e | eVisible e = [eTile e]
              | otherwise = []

renderHero :: Coord -> Tile
renderHero c = Tile { tCharacter = '@', tSgr = [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ], tPosition = c }

renderExit :: Coord -> Tile
renderExit c = Tile {
    tCharacter = '>'
  , tSgr = [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue
           ]
  , tPosition = c
  }

renderObstacle :: ObstacleTile -> Tile
renderObstacle = oTile

gameLoop :: MonadIO m => [Tile] -> World -> m ()
gameLoop prevTiles w = do
  let thisTiles = renderWorld (updateEnemyVisibility w)
  tileDiff prevTiles thisTiles
  --mapM_ drawTile thisTiles
  if wHero w == wExit w then clearScreen >> setCursorPosition (0,0) >> putStrLn "You won!" else do
    input <- getInput
    case input of
      Exit -> return ()
      _    -> do
        let enemyResult = updateEnemies w
        if uerGameOver enemyResult
           then clearScreen >> setCursorPosition (0,0) >> putStrLn "Game over!"
           else do
             let inputResult = handleDir (uerWorld enemyResult) input
             if isJust (enemyAt inputResult (wHero w))
                then clearScreen >> setCursorPosition (0,0) >> putStrLn "Game over!"
                else gameLoop thisTiles inputResult

drawTile :: MonadIO m => Tile -> m ()
drawTile t = do
  setCursorPosition (tPosition t)
  setSGR (tSgr t)
  putStr [tCharacter t]

clearTile :: MonadIO m => Tile -> m ()
clearTile t = do
  setCursorPosition (tPosition t)
  setSGR [SetConsoleIntensity NormalIntensity,SetColor Foreground Vivid White]
  putStr " "

drawHero :: MonadIO m => Coord -> m ()
drawHero c = drawTile Tile {
    tCharacter = '@'
  , tSgr = [ SetConsoleIntensity BoldIntensity
           , SetColor Foreground Vivid Blue
           ]
  , tPosition = c
  }

getInput :: MonadIO m => m Input
getInput = do
  char <- getChar
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

data UpdateEnemyResult = UpdateEnemyResult {
    uerWorld :: World
  , uerGameOver :: Bool
  }

updateEnemyVisibility :: World -> World
updateEnemyVisibility w = let os = obstaclesAsSet w
                          in foldr (updateEnemyVisibility' os) w (wEnemies w)
  where updateEnemyVisibility' os e w' = w' {
          wEnemies = replaceBy (wEnemies w') ((== (tPosition . eTile) e). tPosition . eTile) (e { eVisible = isNothing (viewObstructed os (wHero w') (tPosition . eTile $ e))})
          }

updateEnemies :: World -> UpdateEnemyResult
updateEnemies ow = let lt = litTiles ow
                   in foldr (updateEnemies' lt) (UpdateEnemyResult ow False) (wEnemies ow)
  where updateEnemies' lt e w = let uer = updateEnemy (uerWorld w) lt e
                                in UpdateEnemyResult { uerWorld = uerWorld uer,uerGameOver = uerGameOver uer || uerGameOver w }

maxFramesSeen :: Int
maxFramesSeen = 3

updateEnemy :: World -> CoordSet -> Enemy -> UpdateEnemyResult
updateEnemy w lt e =
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
      newCharacter = if newAggro then '!' else (head . show) newFramesSeen
      newTile = (eTile e) { tPosition = newPosition,tCharacter = newCharacter }
      newCurrentWalk = if isAtTurningPoint
                       then 0
                       else eCurrentWalk e + 1
      newEnemy = Enemy newTile newAggro newWalkingDir (eWalkingRadius e) newCurrentWalk newFramesSeen newVisible
  in UpdateEnemyResult {
         uerWorld = w {
            wEnemies = replaceBy (wEnemies w) ((== (tPosition . eTile) e). tPosition . eTile) newEnemy
            }
       , uerGameOver = playerAt w newPosition
       }

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
        newCoord = if insideBounds (wViewport w) newCoord' then case obstacleAt w newCoord' of
                     Nothing -> newCoord'
                     Just _ -> oldCoord
                   else oldCoord
