{-# Language RecordWildCards, FunctionalDependencies, DuplicateRecordFields, TemplateHaskell, FlexibleInstances, DeriveAnyClass, TypeSynonymInstances, MultiParamTypeClasses #-}

--for generic-lens
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Types where
import Data.Types.Isomorphic
import Data.List
import Data.Csv
import GHC.Generics(Generic)
import Data.Text (Text)
import Control.Lens hiding ((.=),to,both)
import Control.Arrow ((>>>),(&&&))
import Data.Matrix hiding (trace,(!))
import Data.Tuple.Extra (both)
import Data.Vector (Vector,(!))
import Geography.Geocoding.Google (geoEncode, geoDecode)
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as B
data Num = Int

data District = District {
   _name :: !Text
 , code :: !Text
 , lat  :: !Double
 , lng  :: !Double
 } deriving (Show, Generic, FromRecord, ToRecord)

data Location = Loc {
   _goal :: !Text
 , _idn  :: !Int
 , _name :: !Text
 , _address :: !Text
 , _gps_coord :: !(Double,Double)
} deriving (Show, Generic, FromRecord)
instance FromField (Double,Double) where
  parseField s = pure $ (read $ B.unpack s :: (Double,Double))

makeFieldsNoPrefix ''Location
{-
instance FromRecord Location where
    parseRecord v
        | length v == 4 = Loc <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> pure Nothing  
        | otherwise     = mzero


instance ToRecord Location where

    toRecord Loc{..} = record [
        toField _goal, toField _idn, toField _name, toField _address, toField _gps_coord]

--    toNamedRecord Loc{..} = namedRecord [
--        "goal" .= _goal, "idn" .= _idn, "name" .= _name, "address" .= _address, "gps_coord" .= _gps_coord]
--
instance ToField (Double,Double) where
    toField (a,b) = B.pack $ show (a,b) 
-}
type Persons = [Person]

data Car = Car {
    origin :: Loc
  , capacity :: Int
  , task :: Persons
  , route :: Route
} deriving (Show)
type Cars = [Car]

data Person = Person {
    _name :: !Text
 ,  _location :: !Int
 } deriving (Show, Generic, FromRecord, ToRecord)

type People = Int
data PickupPoint = PP Loc People deriving Show
newtype Queue = Q [Loc] deriving Show
instance Monoid Queue where
  mempty = Q []
  (Q xs) `mappend` (Q ys) = Q (xs ++ ys) 

instance Injective [PickupPoint] Queue where
  to ((PP loc ppl):xs) = (Q $ replicate ppl loc) `mappend` to xs
  to [] = Q [] 
instance Injective Queue [PickupPoint] where
  to (Q xs) = PP <$> (nub xs) <*> (map length $ group xs) 

showPickupPoint :: PickupPoint -> String
showPickupPoint (PP l ppl) = "go to " ++ show l ++ " and pickup " ++ show ppl ++ " people\n"

type From = Loc
type Seats = Int

type Loc = Int
type Origin = Loc
type Destination = Loc
type Stops = [Loc]
data Route = Route Origin Stops Destination deriving Show
type Path = [(Loc,Loc)]

routeToList :: Route -> [Int]
routeToList (Route o ss d) = [o]++ss++[d]

routeToPath :: Route -> Path
routeToPath r = uncurry zip $ (init &&& tail) $ routeToList r

getDistance :: Matrix Double -> Path -> Double
getDistance m p = sum $ map (\(x,y) -> getElem (x+1) (y+1) m) p
 
mkDistanceMatrix districts = let l = length districts in matrix l l (countMatrix districts) 

defaultDistanceMatrix = matrix 10 10 (countDefaultMatrix)

countDefaultMatrix :: (Int, Int) -> Double 
countDefaultMatrix (x,y) = distance (fromIntegral x,fromIntegral x) (fromIntegral y,fromIntegral y) 

distance :: (Double,Double) -> (Double,Double) -> Double
distance (a,b) (c,d) = sqrt ((abs (a-c))^2 + (abs(b-d))^2)

countMatrix :: Vector District -> (Int, Int) -> Double
countMatrix ds (x,y) = let
  in uncurry distance $ both (lat &&& lng) ((ds!(x-1)), (ds!(y-1)))

