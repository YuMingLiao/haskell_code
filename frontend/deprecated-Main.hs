{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Reflex.Dom (run)
import Reflex.Dom.Core hiding (Click,Drop)
import GoogleMapsReflex
import GHCJS.DOM.Types (JSM)
import Data.Functor
import Data.Map
import Location
import Types

main :: IO ()
main = do
  locs <- getLocations
  let !config = makeConfig locs
  run $ mapsApp config

mapsApp :: Config Int -> JSM ()
mapsApp config = mainWidget (exampleMapsWidget config)

exampleMapsWidget :: MonadWidget t m => Config Int -> m ()
exampleMapsWidget config = do
  pb <- getPostBuild >>= delay 10 --wait rendering
  configDyn <- holdDyn config (pb $> config)

  --Make the dom element for the map
  (Element _ mapEl, _) <- elAttr' "div" ("style" =: "width: 500px; height: 300px;") blank

  --Create the widget
  maps <- googleMaps mapEl (ApiKey "AIzaSyAXn2V4wLK1QeU6YEFBFCApDQBPyajr8d0") configDyn

  --Get the click event on the map
  mc <- mapEvent Click maps 

  d <- holdDyn "" (const "Clicked" <$> mc)
  el "div" $ dynText d

  return ()


makeConfig :: [Location] -> Config Int
makeConfig locs = def {
  _config_markers = fromList $ zipWith locToMarker locs [0..length locs]
}
locToMarker :: Location -> Int -> (Int, MarkerOptions)
locToMarker Loc{..} num = (num, def {
  _markerOptions_position = LatLng (fst _gps_coord) (snd _gps_coord),
  _markerOptions_title = _name,
  _markerOptions_animation = Just Drop
})
{-
data Location = Loc {
   _goal :: !Text
 , _idn  :: !Int
 , _name :: !Text
 , _address :: !Text
 , _gps_coord :: !(Double,Double)
} deriving (Show, Generic,FromRecord,ToRecord)
-}
