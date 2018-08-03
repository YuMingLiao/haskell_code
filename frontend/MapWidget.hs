{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}


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
import TypeClass

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



