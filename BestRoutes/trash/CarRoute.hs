{-# Language Strict, TypeSynonymInstances, OverloadedStrings,DeriveFunctor #-}

module CarRoute where

import Data.List hiding (group)
import Data.Text (Text)
import Data.Matrix hiding (trace)
import Data.Ord
import Debug.Trace
import Text.CSV
import Data.Permute
import qualified Group as G
import Data.Maybe


locNum = 4
{-
cars :: [Car]
cars = [(1,10),(2,3),(3,4)]

points :: [PickupPoint]
points = [(1,9),(2,3),(3,1)]

end = 4
distanceMatrix = matrix locNum locNum (countDistance) 
countDistance (x,y) | x == y = 0
                    | otherwise = abs (x-y)
-}


best1 :: [PickupPoint] -> [Car] -> Matrix Double -> Loc -> [[PickupPoint]]
best1 points cars dstMtrx end = let
  totalDist = sum . map (sum . dist)
  dist = map (\(x,y) -> getElem (x+1) (y+1) dstMtrx)
  carsRoute q cars' = map toGroup . snd $ mapAccumL chunk q cars'
  in minimumBy (comparing totalDist) $ 
    [ map (toPath . (++[end]) . map (fst)) (carsRoute q cars) 
    | q <- toQueue <$> permutations points]


trace' str a = trace (str ++ ": " ++ show a ++"\n") a

best :: [PickupPoint] -> [Car] -> Matrix Double -> Loc -> [[PickupPoint]]
best points cars dstMtrx end = let
  !length' = traceShowId $ length $ head $ toQueue <$> permutations points
  !ks' = trace' "ks" (map (\(x,y)->y) cars) 
  !ks = [1,2,3,4]
  !g = G.group (sum ks) ks
  !a = trace' "group" $ g == (fromJust $ G.next g)
  in [[]] 

chunk :: [Loc] -> Car -> ([Loc],[Loc])
chunk q c@(from,s) = (drop s q, from : take s q)

toPath :: [Loc] -> [(Loc,Loc)]
toPath (x:y:ys) = (x,y) : toPath (y:ys)
toPath (x:[]) = []
toPath [] = []

toQueue :: [PickupPoint] -> [Loc]
toQueue (x@(loc,num):xs) = replicate num loc ++ toQueue xs
toQueue [] = []

toGroup :: [Loc] ->[(Loc,People)]
toGroup (x:xs) = case dropWhile (==x) xs of
                     [] -> [(x, (1+) (length $ takeWhile (==x) xs))]
                     ys  -> (x, (1+) (length $ takeWhile (==x) xs)):toGroup ys
toGroup []     = []

-- ping (guard) for user custom behaviour.
--
-- people permutation by loc
-- divide by car seats and generate routes
   -- (choose user favor routes)
-- count weight and produce the best routes.
   -- user decide last minute
-- generate reports.
--
