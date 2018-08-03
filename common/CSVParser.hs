{-# LANGUAGE OverloadedStrings #-}

module CSVParser where
import Data.Csv
import qualified Data.ByteString.Lazy as BS 
import Data.Text hiding (map)
import qualified Data.Vector as V
import Data.Either
import Types

getPeople :: String -> IO [Person]
getPeople filename = do
  txt <- BS.readFile filename
  return $ V.toList $ either error (id) $ (decode NoHeader txt :: Either String (V.Vector Person)) 

getDistricts :: String -> IO [District]
getDistricts filename = do
  txt <- BS.readFile filename
  return $ V.toList $ either error (id) $ (decode NoHeader txt :: Either String (V.Vector District)) 

getLocations :: String -> IO [Location]
getLocations filename = do
  txt <- BS.readFile filename
  return $ V.toList $ either error (id) $ (decode NoHeader txt :: Either String (V.Vector Location)) 

