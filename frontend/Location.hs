{-# Language TemplateHaskell,DeriveGeneric,DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Location where
import Data.Csv
import Debug.Trace
import qualified Data.ByteString.Lazy as BS 
import Data.Text hiding (map,filter)
import qualified Data.Vector as V
import Data.Either
import GHC.Generics(Generic)
--import Text.Show.Unicode
import Text.Pretty.Simple
import Control.Arrow
import Data.List as L
import Types
import System.IO
import Control.Lens
import Data.Function (on)
import Data.Ord
import Data.Maybe (fromJust)
import Control.Monad
import Data.Either.Combinators (rightToMaybe)

getLocations :: IO [Location]
getLocations = do
  mapM_ (flip hSetEncoding utf8) [stdin,stdout,stderr]
  txt <- BS.readFile "locations_gps.csv"
  let locations = either error (id) $ (decode NoHeader txt :: Either String (V.Vector Location)) 
  return $ V.toList locations

