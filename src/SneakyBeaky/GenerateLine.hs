module SneakyBeaky.GenerateLine(line) where

import           ClassyPrelude
import           Prelude                                 ()
import           Linear.V2
import           Control.Lens                            ((^.))

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: (Ord a, Num a) => a -> a -> a -> [a]
balancedWord p q eps | eps + p < q = 0 : balancedWord p q (eps + p)
balancedWord p q eps               = 1 : balancedWord p q (eps + p - q)

line :: (Ord a, Num a) => V2 a -> V2 a -> [V2 a]
line start end = takeWhile (/= end) (line' start end) ++ [end]

-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
line' :: (Ord a, Num a) => V2 a -> V2 a -> [V2 a]
line' v1 v2 =
  let d = v2 - v1
      xyStep b v = V2 ((v ^. _x) + signum (d ^. _x)) ((v ^. _y) + signum (d ^. _y) * b)
      yxStep b v = V2 ((v ^. _x) + signum (d ^. _x) * b) ((v ^. _y) + signum (d ^. _y))
      (p, q, step) | abs (d ^. _x) > abs (d ^. _y) = (abs (d ^. _y), abs (d ^. _x), xyStep)
                   | otherwise       = (abs (d ^. _x), abs (d ^. _y), yxStep)
      walk w xy = xy : walk (unsafeTail w) (step (unsafeHead w) xy)
  in  walk (balancedWord p q 0) v1

