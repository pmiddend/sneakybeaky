module Main where

import           Control.Applicative    ((<$>), (<|>))
import           Data.Graph.AStar       (aStar)
import           Data.List              (find)
import           Data.Maybe             (isJust, isNothing)
import           Data.Monoid            ((<>))
import qualified Data.Set               as Set
import           Prelude                hiding (Either (..), getChar, putStr,
                                         putStrLn)
--import qualified Data.Set as Set
import qualified Data.HashMap.Strict    as Map
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

initialWorld :: [ObstacleTile] -> [LightSource] -> Coord -> Rect -> World
initialWorld obstacles lightSources exit viewport = World {
    wHero = (0,0)
  , wObstacles = obstacles
  , wExit = exit
  , wLightSources = lightSources
  , wViewport = viewport
  , wEnemies = [Enemy (Tile (5,5) '0' (mkColorPair White Transparent)) False (1,0) 10 0 0 False]
  }

main :: IO ()
main = do
  let standardViewport = mkRectPosDim (0,0) (80,25)
  run standardViewport $ do
      obstacles <- evalRandIO $ generateObstacles standardViewport
      let obstaclePositions = (tPosition . oTile) <$> obstacles
      exit <- evalRandIO $ generateNoConflict standardViewport obstaclePositions
      lightSourcePosition <- evalRandIO $ generateNoConflict standardViewport obstaclePositions
      gameLoop (initialWorld obstacles [LightSource lightSourcePosition 200] exit standardViewport)

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
                 litFilter lite = isNothing (foldr ((<|>) . (\l -> viewObstructed obstacleTiles (lsPosition l) lite)) Nothing lights)
             in Set.filter litFilter lit

renderWorld :: World -> Rect -> [Tile]
renderWorld w viewport =
  let obstacleTiles = obstacleTilesAsMap w
      enemyTiles = Map.fromList $ map tileToAssoc $ concatMap renderEnemy (wEnemies w)
      realTiles = obstacleTiles <> enemyTiles <> Map.fromList (map tileToAssoc [renderHero (wHero w),renderExit (wExit w)])
      renderedLit = (Map.fromList . map (\c -> (c,renderLit c)) . Set.toList) (litTiles w)
  in (filter (insideRect viewport . tPosition) . map snd . Map.toList) (realTiles <> renderedLit)

renderLit :: Coord -> Tile
-- renderLit c = Tile { tCharacter = '\x2591', tSgr = [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Blue ], tPosition = c }
-- renderLit c = Tile { tCharacter = '.', tSgr = [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Blue ], tPosition = c }
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
--   , tSgr = [ SetConsoleIntensity BoldIntensity
--            , SetColor Foreground Vivid Blue
--            ]
  , tPosition = c
  , tColor = mkColorPair Blue Transparent
  }

renderObstacle :: ObstacleTile -> Tile
renderObstacle = oTile

moore :: Coord -> Set.Set Coord
moore (x,y) = Set.fromList [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]

calculateOptimalPath :: World -> Coord -> Coord -> Maybe [Coord]
calculateOptimalPath w from to = aStar neighbors distance heuristic isGoal from
  where obstacles = obstaclesAsSet w
        neighbors c = moore c `Set.difference` obstacles
        distance x y | x == y = 0
                     | otherwise = 1
        heuristic (x,y) = (x-fst to)*(x-fst to)+(y-snd to)*(y-snd to)
        isGoal c = c == to

{-drawOptimalPath w = case calculateOptimalPath w of
    Nothing -> putStrLn "Oh crap"
--     Just p -> setCursorPosition (0,0) >> putStr ("|" <> (show . head) p <> "|")--mapM_ (drawTile . (\c -> Tile c 'P' [])) p
    Just p -> mapM_ (drawTile . (\c -> Tile c 'P' [])) p-}

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
updateEnemyAggro w _ e = case calculateOptimalPath w ((tPosition . eTile) e) (wHero w) of
  Nothing -> e
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
      newCharacter = if newAggro then '!' else (head . show) newFramesSeen
      newTile = (eTile e) { tPosition = newPosition,tCharacter = newCharacter }
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
