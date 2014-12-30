module SneakyBeaky.Coord where

type Coord = (Int, Int)
type Dim = (Int, Int)

pairPlus :: Num a => Coord -> Coord -> Coord
pairPlus (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

pairMinus :: Num a => Coord -> Coord -> Coord
pairMinus (a1, a2) (b1, b2) = (a1 - b1, a2 - b2)

pairDiv :: Num a => Int -> Coord -> Coord
pairDiv n (a1, a2) = (div a1 n, div a2 n)

cog :: [Coord] -> Coord
cog xs = (pairDiv (length xs)) (foldl1 pairPlus xs)
