module SneakyBeaky.List where

import ClassyPrelude
import qualified Data.List.NonEmpty as NE
import Prelude()

replace :: Eq a => [a] -> a -> a ->[a]
replace [] _ _ = []
replace (x:xs) old new | x == old = new : replace xs old new
                       | otherwise = x : replace xs old new

replaceBy :: [a] -> (a -> Bool) -> a ->[a]
replaceBy [] _ _ = []
replaceBy (x:xs) f new | f x = new : replaceBy xs f new
                       | otherwise = x : replaceBy xs f new

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

groupByEquals :: (Foldable f, Eq b) => (a -> b) -> f a -> [(b, NE.NonEmpty a)]
groupByEquals f xs = fmap (\l -> (f (NE.head l),l)) . NE.groupBy ((==) `on` f) $ xs
