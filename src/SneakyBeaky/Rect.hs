module SneakyBeaky.Rect where

import SneakyBeaky.Coord

data Rect = Rect {
    rTopLeft :: Coord
  , rDim :: Coord
  }

rBottomRight :: Rect -> Coord
rBottomRight (Rect p d) = p `pairPlus` d

mkRectPosDim :: Coord -> Dim -> Rect
mkRectPosDim = Rect

mkRectFromCorners :: Coord -> Coord -> Rect
mkRectFromCorners p1 p2 = Rect p1 (p2 `pairMinus` p1)

