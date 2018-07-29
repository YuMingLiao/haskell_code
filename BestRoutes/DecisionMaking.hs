{-# Language OverloadedStrings #-}
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List as List
import Data.Monoid
import Text.Printf
import Data.Text (Text)
import System.Environment

-- Car Seat num
data Seat = Seat Int deriving (Eq, Ord)

instance Show Seat where
  show (Seat n) = printf "%03d" n

data Person = Person
  { firstName :: String
  , lastName :: String
  , gender :: Gender
  } deriving (Eq)

data Gender = Male | Female deriving (Eq, Show)

instance Show Person where
  show (Person fName lName g) = fName ++ ' ':lName ++ " (" ++ show g ++ ")"
  
type Passengers = Map.Map Seat Person

type Sedan = Passengers

type MiniBus = Passengers

type TourBus = Passengers

data Location = Taipei | Yunlin

data CarType = Sedan | MiniBus | TourBus

data Car = Car {
  name :: Text
 ,location :: Location
 ,carType :: CarType
}

maxbound :: CarType -> Int
maxbound Sedan = 4
maxbound MiniBus = 8
maxbound TourBus = 40

carList = [Car "1" Taipei MiniBus, Car "2" Taipei MiniBus, Car "3" Yunlin MiniBus]

data Condition = Enough | NeedSupport | NeedTourBus deriving Show

judge :: Int -> Condition
judge n | n <= 0 = Enough
        | n > 0 && n < (maxbound TourBus) `div` 2 = NeedSupport
        | n >= (maxbound TourBus) `div` 2 = NeedTourBus

main = do
  args <- getArgs
  let people_number = read (args !! 0) :: Int
  let max = sum $ map (maxbound.carType) carList
  print $ judge $ people_number - max
  
