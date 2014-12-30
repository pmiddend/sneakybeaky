module SneakyBeaky.Coord where

type Coord = (Int, Int)
type Dim = (Int, Int)

pairPlus :: Num a => (a,a) -> (a,a) -> (a,a)
pairPlus (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

pairMinus :: Num a => (a,a) -> (a,a) -> (a,a)
pairMinus (a1, a2) (b1, b2) = (a1 - b1, a2 - b2)
