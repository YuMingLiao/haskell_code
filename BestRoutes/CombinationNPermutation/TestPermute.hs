{-# Language OverloadedStrings #-}
module TestPermute where

import Prelude hiding (foldl1)
import Group1
import Permutations
import Types
import Data.List hiding (group,foldl1)
import Data.List.Extra (minimumOn)
import Conduit
import Data.Conduit.Combinators hiding (sum,map,length,print,concatMap,replicate)
import Control.Arrow
import Data.Function (on)
import Data.Maybe
import Data.Text (pack)
import Control.Monad (liftM2,when)
import Text.Pretty.Simple

car1 = Car 0 4 [] (Route 0 [] 4) 
car2 = Car 1 4 [] (Route 1 [] 4) 
car3 = Car 0 2 [] (Route 0 [] 4) 

defaultCars = [car1,car2,car3]

defaultPeople = zipWith Person (map (pack.(:"")) ['A'..'J']) [1,2,3,1,2,3,1,1,1,1]

data Environment = Env {
  cars :: Cars
 ,people :: Persons 
}

data Report = Report {
  cars :: Cars
 ,people :: Persons
 ,left :: Persons
}

eval :: a -> (a -> [b]) -> b
eval = undefined

type Locs = [Loc]
changeStops :: Route -> Locs -> Route
changeStops (Route o stops d) stops'= Route o stops' d

mkCarPlan :: Car -> Persons -> Car
mkCarPlan (Car a b _ r) ps = Car a b ps (changeStops r (decideRoute ps))

permuteStops :: Route -> [Route]
permuteStops (Route o stops d) = (\s-> Route o s d) <$> permutationsOf stops

findShortestRoute :: Route -> (Route, Double)
findShortestRoute r = 
  let r' = minimumOn (getDistance defaultDistanceMatrix . routeToPath) . permuteStops $ r
  in (r', getDistance defaultDistanceMatrix $ routeToPath r')
{-
type Routes = [Route]
findShortestRoutes :: [Routes] -> Routes
findShortestRoutes = minimumOn (sum . map snd . map findShortestRoute)
-}
getRoutesDistance :: Cars -> Double
getRoutesDistance cs = sum $ map (getDistance defaultDistanceMatrix . routeToPath) $ map route cs

unfoldGroup :: Maybe (Group Person) -> Maybe ([Persons],Maybe (Group Person))
unfoldGroup maybeGroup = maybe Nothing produce maybeGroup
  where produce g = Just (elems g, next g) 

mkCarsPlan :: Cars -> [Persons] -> Cars
mkCarsPlan cs ps = zipWith mkCarPlan cs ps

mapMbTupleFst :: Maybe (a,c) -> (a -> b) -> Maybe (b,c)
mapMbTupleFst (Just (a,b)) f = Just (f a, b)
mapMbTupleFst Nothing f = Nothing

decideRoute :: Persons -> Locs
decideRoute ppl = nub $ map location ppl

unfoldCarsPlan :: Cars -> Maybe (Group Person) -> Maybe (Cars, Maybe (Group Person))
unfoldCarsPlan cs mbGroup = mapMbTupleFst (unfoldGroup mbGroup) (mkCarsPlan cs)

minBy f x y = case f x > f y of
                     True  -> y
                     False -> x

--limit car seats to match people No.
pile :: Int -> [Int] -> [[Int]]
pile n ks = 
  let m = length ks in
  [ zipWith (-) ks xs | xs <- sequence $ map zeroTo ks, sum xs == n]

zeroTo :: Int -> [Int]
zeroTo n = [0..n]

main :: IO ()
main = do 
  let cars = defaultCars
  let routes = route <$> cars
  let capacities = capacity <$> cars
  print $ length defaultPeople
  when (length defaultPeople /= sum (capacity <$> cars)) (error "capacities and people num doesn't match.")
  let g = group (length defaultPeople) capacities defaultPeople
  let carSetting = (g, cars)

  pPrint $ id &&& getRoutesDistance $ fromJust $ shortestCarsPlan cars g 

shortestCarsPlan cars g = runConduitPure $ unfold (unfoldCarsPlan cars) (Just g) .| foldl1 (minBy getRoutesDistance)

