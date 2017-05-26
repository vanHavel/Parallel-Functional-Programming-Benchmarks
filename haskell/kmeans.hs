module Kmeans where

import Control.Monad.Par

data Point = Point {
  x :: Double,
  y :: Double
}

square :: Double -> Double
square a = a * a

distance :: Point -> Point -> Double
distance p q = square (x p - x q) + square (y p - y q)

closest :: [Double] -> Int
closest xs = snd . minimum $ zip xs [0..length xs]

assign :: [Point] -> [Point] -> [Int]
assign means = map (\p -> closest (map (`distance` p) means))

passign :: [Point] -> [Point] -> Int -> Par [Int]
passign means ps chunksize | length ps <= chunksize = return $ assign means ps
                           | otherwise              = do
                               let (part, rest) = splitAt chunksize ps
                               ipart <- spawnP (assign means part)
                               crest <- passign means rest chunksize
                               cpart <- get ipart
                               return $ cpart ++ crest
