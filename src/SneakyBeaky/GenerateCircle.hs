module SneakyBeaky.GenerateCircle where

import           System.Console.SneakyTerm.PointInt
import Linear.V2
import Data.List(unfoldr)

-- Courtesy of http://rosettacode.org/wiki/Bitmap/Midpoint_circle_algorithm#Haskell
generateCirclePoints :: PointInt -> Int -> [PointInt]
generateCirclePoints (V2 x0 y0) radius
  -- Four initial points, plus the generated points
  = (V2 x0 ( y0 + radius )) : (V2 x0 ( y0 - radius )) : (V2 ( x0 + radius ) y0) : (V2 ( x0 - radius ) y0) : points
    where
      -- Creates the (x, y) octet offsets, then maps them to absolute points in all octets.
      points = concatMap generatePoints $ unfoldr step initialValues
      generatePoints (V2 x y)
        = [V2 (xop x0 x') (yop y0 y') | (x', y') <- [(x, y), (y, x)], xop <- [(+), (-)], yop <- [(+), (-)]]
 
      -- The initial values for the loop
      initialValues = (1 - radius, 1, (-2) * radius, 0, radius)
 
      -- One step of the loop. The loop itself stops at Nothing.
      step (f, ddf_x, ddf_y, x, y) | x >= y = Nothing
                                   | otherwise = Just ((V2 x' y'), (f', ddf_x', ddf_y', x', y'))
                                     where
                                       (f', ddf_y', y') | f >= 0 = (f + ddf_y' + ddf_x', ddf_y + 2, y - 1)
                                                        | otherwise = (f + ddf_x, ddf_y, y)
                                       ddf_x' = ddf_x + 2
                                       x' = x + 1
