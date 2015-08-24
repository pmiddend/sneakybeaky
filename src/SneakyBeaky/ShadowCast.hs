{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module SneakyBeaky.ShadowCast(castShadow,execAndPrintCast,printShadowCast) where

import           System.Console.SneakyTerm.PointInt
import           Linear.V2
import SneakyBeaky.List(groupByEquals)
import           ClassyPrelude
import qualified Data.List.NonEmpty as NE
import           Prelude                                 ()
import           Control.Lens                            (Getter,lens,Lens',_2,_1,to, (^.),view,(&),ix,(.~))

data Neighbors a = Neighbors (Maybe a) a (Maybe a) deriving(Functor)

zipWithLeft :: (Maybe b -> b -> c) -> [b] -> [c]
zipWithLeft f xs = zipWith f (Nothing : (Just <$> xs)) xs

zipWithRight :: (a -> Maybe a -> c) -> [a] -> [c]
zipWithRight f xs = zipWith f xs ((Just <$> drop 1 xs) <> [Nothing])

leftNeighbor :: Lens' (Neighbors a) (Maybe a)
leftNeighbor = lens (\(Neighbors l _ _) -> l) (\(Neighbors _ x r) l -> Neighbors l x r)

rightNeighbor :: Lens' (Neighbors a) (Maybe a)
rightNeighbor = lens (\(Neighbors _ _ r) -> r) (\(Neighbors l x _) r -> Neighbors l x r)

atElem :: Lens' (Neighbors a) a
atElem = lens (\(Neighbors _ x _) -> x) (\(Neighbors l _ r) x -> Neighbors l x r)

withNeighbors :: [b] -> [Neighbors b]
withNeighbors xs = zipWithRight (\(left,x) right -> Neighbors left x (snd <$> right)) (zipWithLeft (,) xs)

data Span a = Span Bool Bool (NE.NonEmpty a)
              deriving(Functor)

leftBlocked :: Getter (Span a) Bool
leftBlocked = to (\(Span l _ _) -> l)

rightBlocked :: Getter (Span a) Bool
rightBlocked = to (\(Span _ r _) -> r)

spanElems :: Getter (Span a) (NE.NonEmpty a)
spanElems = to (\(Span _ _ e) -> e)

deriving instance Show a => Show (Span a)


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

castShadowOctant :: forall a.( Show a,RealFrac a ) =>
  V2 a -- ^ Starting point
  -> a -- ^ Start slope
  -> a -- ^ End slope
  -> (PointInt -> Bool) -- ^ Tile position to translucency
  -> [PointInt] -- ^ List of lit tiles
castShadowOctant (V2 sx sy) ss se isBlocked =
  let
    -- Integral version of the start vector
    (V2 sxi syi) = round <$> V2 sx sy
    -- Non-blocked spans
    spanResults :: [Span PointInt]
    spanResults = ((`V2` syi) <$>) <$> ( spans (isBlocked . (`V2` syi)) sxi (round (se * sy)) :: [Span Int])
    recursion :: Span PointInt -> [PointInt]
    recursion s =
      let
        f = fromIntegral <$> NE.head (s ^. spanElems) :: V2 a
        fs = if s ^. leftBlocked then slope f else ss
        l = fromIntegral <$> NE.last (s ^. spanElems) :: V2 a
        ls = if s ^. rightBlocked then slope l else se
      in
        castShadowOctant (f + V2 fs 1) fs ls isBlocked
  in
    (concatMap NE.toList ( view spanElems <$> spanResults )) <> (concatMap recursion spanResults)

castShadow :: (PointInt -> Bool) -> [PointInt]
castShadow isBlocked =
  let
    castOctant f = f <$> castShadowOctant (V2 1 1) ( 1 :: Float ) 0 (isBlocked . f)
  in
   castOctant (\(V2 x y) -> V2 x y) <>
   castOctant (\(V2 x y) -> V2 (-x) y) <>
   castOctant (\(V2 x y) -> V2 x (-y)) <>
   castOctant (\(V2 x y) -> V2 (-x) (-y)) <>
   castOctant (\(V2 x y) -> V2 y x) <>
   castOctant (\(V2 x y) -> V2 (-y) x) <>
   castOctant (\(V2 x y) -> V2 y (-x)) <>
   castOctant (\(V2 x y) -> V2 (-y) (-x))

printShadowCast :: [PointInt] -> IO ()
printShadowCast r =
  let
    ls :: [(Int,NE.NonEmpty PointInt)]
    ls = groupByEquals (view _y) (sortBy (comparing (view _y)) r)
    sortedLines :: [(Int,NE.NonEmpty PointInt)]
    sortedLines = sortBy (comparing fst) ls
    numLines = length sortedLines
    mapLine :: (Int,NE.NonEmpty PointInt) -> Text
    mapLine (_,points) = foldr (\(V2 x _) s -> s & ix (numLines - x - 1) .~ '.') (replicate numLines ' ') points
    mappedLines :: [Text]
    mappedLines = mapLine <$> sortedLines
  in
    mapM_ putStrLn (reverse mappedLines)

execAndPrintCast :: (PointInt -> Bool) -> Text
execAndPrintCast b =
  let
    r = castShadow b
    matrix = foldr (\(V2 x y) f -> f & ix (y + 6) . ix (x + 6) .~ '.') (replicate 13 (replicate 13 ' ')) r
  in
    unlines . reverse $ matrix
