module Main where

import Prelude hiding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Monad.IO.Class(MonadIO,liftIO)
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
import SneakyBeaky.Generation


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

litTiles :: [LightSource] -> CoordSet
litTiles = Set.unions . map lightTiles

gameTitle :: String
gameTitle = "sneakybeaky"

initialWorld :: [ObstacleTile] -> [LightSource] -> Coord -> Rect -> World
initialWorld obstacles lightSources exit viewport = World {
    wHero = (0,0)
  , wObstacles = obstacles
  , wExit = exit
  , wLightSources = lightSources
  , wViewport = viewport
  , wEnemies = [Enemy (Tile (5,5) 'E' []) False (1,0) 10 0 0]
  }

main :: IO ()
main = bracket_ (hSetEcho stdin False >> hSetBuffering stdin  NoBuffering >> hSetBuffering stdout NoBuffering >> hideCursor) (showCursor >> hSetEcho stdin True) $ do
  let standardViewport = mkRectPosDim (0,0) (80,25)
  obstacles <- evalRandIO $ generateObstacles standardViewport
  exit <- evalRandIO (generateNoConflict standardViewport (map (tPosition . oTile) obstacles))
  setTitle gameTitle
  liftIO clearScreen
  gameLoop [] (initialWorld obstacles [LightSource (10,10) 200] exit standardViewport)

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

insideLight :: Coord -> LightSource -> Bool
insideLight (x,y) (LightSource (lx,ly) r) = (x-lx)*(x-lx) + (y-ly)*(y-ly) <= r

viewObstructed :: Map.HashMap Coord Tile -> LightSource -> Coord -> Maybe Coord
viewObstructed obstacles light x = find (`Map.member` obstacles) (line (lsPosition light) x)

tileDiff :: MonadIO m => [Tile] -> [Tile] -> m ()
tileDiff before after = do
  let toClear = before \\ after
      toAdd = after \\ before
  mapM_ clearTile toClear
  mapM_ drawTile toAdd

tileToAssoc :: Tile -> (Coord,Tile)
tileToAssoc t = (tPosition t,t)

renderWorld :: World -> [Tile]
renderWorld w = let obstacleTiles = Map.fromList $ map (tileToAssoc . renderObstacle) (wObstacles w)
                    enemyTiles = Map.fromList $ map (tileToAssoc . renderEnemy) (wEnemies w)
                    realTiles = obstacleTiles <> enemyTiles <> Map.fromList (map tileToAssoc [renderHero (wHero w),renderExit (wExit w)])
                    lit = litTiles (wLightSources w)
                    --tilePositions = (Set.fromList . map tPosition) realTiles
                    --obstaclePositions = (Set.fromList . map tPosition) obstacleTiles
                    --litFilter lite = lite `Map.member` realTiles
                    litFilter lite = isNothing (viewObstructed obstacleTiles (head (wLightSources w)) lite)
                    --litFilter = const True
                    renderedLit = (Map.fromList . map (\c -> (c,renderLit c)) . filter litFilter . Set.toList) lit
                    --renderedLit = []
                in (map snd . Map.toList) (realTiles <> renderedLit)

renderLit :: Coord -> Tile
renderLit c = Tile { tCharacter = '\x2591', tSgr = [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Blue ], tPosition = c }

renderEnemy :: Enemy -> Tile
renderEnemy = eTile

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
  let thisTiles = renderWorld w
  tileDiff prevTiles thisTiles
  --mapM_ drawTile thisTiles
  if wHero w == wExit w then liftIO (clearScreen >> putStrLn "You won!") else do
    input <- getInput
    case input of
      Exit -> return ()
      _    -> do
        let enemyResult = updateEnemies (handleDir w input)
        if uerGameOver enemyResult
           then liftIO (clearScreen >> putStrLn "Game over!")
           else gameLoop thisTiles (uerWorld enemyResult)

drawTile :: MonadIO m => Tile -> m ()
drawTile t = do
  liftIO $ setCursorPosition (snd (tPosition t)) (fst (tPosition t))
  liftIO $ setSGR (tSgr t)
  liftIO $ putStr [tCharacter t]

clearTile :: MonadIO m => Tile -> m ()
clearTile t = do
  liftIO $ setCursorPosition (snd (tPosition t)) (fst (tPosition t))
  liftIO $ setSGR [SetConsoleIntensity NormalIntensity,SetColor Foreground Vivid White]
  liftIO $ putStr " "

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
  char <- liftIO getChar
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

replace :: Eq a => [a] -> a -> a ->[a]
replace [] _ _ = []
replace (x:xs) old new | x == old = new : replace xs old new
                       | otherwise = x : replace xs old new

replaceBy :: [a] -> (a -> Bool) -> a ->[a]
replaceBy [] _ _ = []
replaceBy (x:xs) f new | f x = new : replaceBy xs f new
                       | otherwise = x : replaceBy xs f new

data UpdateEnemyResult = UpdateEnemyResult {
    uerWorld :: World
  , uerGameOver :: Bool
  }

updateEnemies :: World -> UpdateEnemyResult
updateEnemies ow = foldr updateEnemies' (UpdateEnemyResult ow False) (wEnemies ow)
  where updateEnemies' e w = let uer = updateEnemy (uerWorld w) e
                             in UpdateEnemyResult { uerWorld = uerWorld uer,uerGameOver = uerGameOver uer || uerGameOver w }

updateEnemy :: World -> Enemy -> UpdateEnemyResult
updateEnemy w e =
  let oldPosition = tPosition (eTile e)
      newPosition' = oldPosition `pairPlus` eWalkingDir e
      isAtTurningPoint = eCurrentWalk e + 1 == eWalkingRadius e
      isObstructed = isJust (obstacleAt w newPosition') || isJust (enemyAt w newPosition')
      isTurning = isAtTurningPoint || isObstructed
      newWalkingDir = if isTurning
                      then pairNegate (eWalkingDir e)
                      else eWalkingDir e
      newPosition = if isObstructed then oldPosition else newPosition'
      newCharacter = tCharacter (eTile e)
      newTile = (eTile e) { tPosition = newPosition,tCharacter = newCharacter }
      newAggro = eAggro e
      newCurrentWalk = if isAtTurningPoint
                       then 0
                       else eCurrentWalk e + 1
      newFramesSeen = eFramesSeen e
      newEnemy = Enemy newTile newAggro newWalkingDir (eWalkingRadius e) newCurrentWalk newFramesSeen
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
