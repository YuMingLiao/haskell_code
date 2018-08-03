{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module CSVParser where
import Data.Csv
import qualified Data.ByteString.Lazy as BS 
import Data.Text hiding (map)
import qualified Data.Vector as V
import Data.Either
import Types
import Control.Lens

getDistricts :: String -> IO ()
getDistricts filename = do
  txt <- BS.readFile filename
  let dList = V.toList $ either error (id) $ (decode NoHeader txt :: Either String (V.Vector District)) 
  let dList' =  map switch dList
  BS.writeFile "district.csv" $ encode $ dList'

switch d =
  let x = d^.lat
      y = d^.lng
  in set lng x (set lat y d)
