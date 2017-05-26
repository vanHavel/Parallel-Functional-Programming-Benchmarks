module Mergesort where

import Control.Monad.Par

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let (ys, zs) = splitAt (length xs `div` 2) xs
             in merge (msort ys) (msort zs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

pmsort :: (Ord a, NFData a) => [a] -> Int -> [a]
pmsort xs cutoff | length xs <= cutoff = msort xs
                 | otherwise           = runPar $ do
  let (ys, zs) = splitAt (length xs `div` 2) xs
  sys <- spawnP (pmsort ys cutoff)
  szs <- spawnP (pmsort zs cutoff)
  gys <- get sys
  gzs <- get szs
  return $ merge gys gzs

bmsort :: (Ord a) => [a] -> [a]
bmsort []  = []
bmsort [x] = [x]
bmsort xs  = let (ys, zs) = splitAt (length xs `div` 2) xs
               in bmerge (bmsort ys) ++ reverse (bmsort zs)

pbmsort :: (Ord a, NFData a) => [a] -> Int -> [a]
pbmsort xs cutoff | length xs <= cutoff = msort xs
                  | otherwise           = runPar $ do
  let (ys, zs) = splitAt (length xs `div` 2) xs
  sys <- spawnP (pbmsort ys cutoff)
  szs <- spawnP (reverse $ pbmsort zs cutoff)
  gys <- get sys
  gzs <- get szs
  return $ pbmerge (gys ++ gzs) cutoff

bmerge :: (Ord a) => [a] -> [a]
bmerge [] = []
bmerge [x] = [x]
bmerge xs = let (ys, zs) = splitAt (length xs `div` 2) xs
                (mins, maxs) = unzip [(min x y, max x y) | (x, y) <- zip ys zs]
                  in bmerge mins ++ bmerge maxs

pbmerge :: (Ord a, NFData a) => [a] -> Int -> [a]
pbmerge xs cutoff | length xs <= cutoff = bmerge xs
                  | otherwise           =
             let (ys, zs) = splitAt (length xs `div` 2) xs
                 (mins, maxs) = unzip [(min x y, max x y) | (x, y) <- zip ys zs]
                  in runPar $ do
                    mmins <- spawnP (pbmerge mins cutoff)
                    mmaxs <- spawnP (pbmerge maxs cutoff)
                    gmins <- get mmins
                    gmaxs <- get mmaxs
                    return $ gmins ++ gmaxs
