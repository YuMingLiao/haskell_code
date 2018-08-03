{-# LANGUAGE OverloadedStrings #-}
import           Reflex.Dom hiding(Click)
import           Data.Time
import           Control.Monad.Trans (liftIO)
import qualified Data.Text as T
import GoogleMapsReflex
import MapWidget
import Data.Map

main :: IO ()
main = mainWidget bodyElement

bodyElement :: MonadWidget t m =>  m()
bodyElement = do
  evIncr <- button "Increase"
  numbs <- foldDyn (+) (0 :: Double)  (1 <$ evIncr)
  display $ zipDynWith (,) numbs numbs
  let latlng = zipDynWith taiwan numbs numbs

  mapWidget' $ mkConfig <$> latlng

taiwan :: Double -> Double -> LatLng
taiwan x y = LatLng (23+x) (120+y)
 
mkConfig :: LatLng -> Config Int
mkConfig ll = def {
    _config_mapOptions = def { _mapOptions_center = LatLng 23 120 }
  , _config_markers = fromList [(0,marker ll)] 
}

mapWidget' :: MonadWidget t m => Dynamic t (Config Int)  -> m ()
mapWidget' configDyn = do
  --pb <- getPostBuild >>= delay 0.1 --wait rendering
  -- configDyn' <- holdDyn config (tag (current configDyn) pb)

  --Make the dom element for the map
  (Element _ mapEl, _) <- elAttr' "div" ("style" =: "width: 500px; height: 600px;") blank

  --Create the widget
  maps <- googleMaps mapEl (ApiKey "AIzaSyAXn2V4wLK1QeU6YEFBFCApDQBPyajr8d0") configDyn

  --Get the click event on the map
  mc <- mapEvent Click maps 

  d <- holdDyn "" (const "Clicked" <$> mc)
  el "div" $ dynText d

  return ()




