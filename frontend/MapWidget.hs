{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}


module MapWidget where
import Reflex.Dom (run)
import Reflex.Dom.Core hiding (Click,Drop)
import GoogleMapsReflex
import GHCJS.DOM.Types (JSM)
import Data.Functor
import Data.Map
import Location
import Types (Location(..))
import GoogleMapsReflex.JSTypes.LatLng

{-
main :: IO ()
main = do
  locs <- getLocations
  let !config = makeConfig locs
  run $ mapsApp config

mapsApp :: Config Int -> JSM ()
mapsApp config = mainWidget (mapWidget config)
-}

mapWidget :: MonadWidget t m => Config Int -> m ()
mapWidget config = do
  pb <- getPostBuild >>= delay 0.1 --wait rendering
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

mapWidget' :: MonadWidget t m => Dynamic t (Config k)  -> m ()
mapWidget' configDyn = do
--  pb <- getPostBuild >>= delay 0.1 --wait rendering
--  configDyn <- holdDyn config (pb $> config)

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

config :: Config Int
config = def {
  _config_markers = fromList [ (0, def {
  _markerOptions_position = LatLng 0 0,
  _markerOptions_title = "title",
  _markerOptions_animation = Just Drop
})] }

def' = def { _markerOptions_animation = Just Drop }

marker :: LatLng -> MarkOptions
marker ll = def' { _markerOptions_position = ll }



