{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
{-# Language GADTs #-}
{-# Language FlexibleContexts #-}

import Reflex.Dom hiding (Click)
import MapWidget
import Common
import Servant.Reflex
import Servant.Reflex.Multi
import Servant.API 
import Data.Proxy
import Types
import qualified Data.Text as T
import Text.Pretty.Simple
import Control.Lens
import GoogleMapsReflex
import Data.Map hiding (map)
import GTable (table, dynTable)
import Data.Functor
import TypeClass

--import Servant.Common.BaseUrl
main :: IO ()
main = mainWidget $ do
  demo
  return ()
--  mapWithConfigDyn

bt2display name api = 
  elClass "div" name $ do
    bt  <- button name
    ev <- fmapMaybe reqSuccess <$> api bt
    display =<< holdDyn 0 ev

bt2Table name api =
  elClass "div" name $ do
    bt  <- button name
    ev <- fmapMaybe reqSuccess <$> api bt
    dynTable name ev

bt_ev name api = do
    bt  <- button name
    ev <- fmapMaybe reqSuccess <$> api bt
    return ev

demo :: forall t m. MonadWidget t m => m ()
demo = do
  let (getint :<|> getplist :<|> getdistricts :<|> getlocations :<|> _) = 
                               client (Proxy :: Proxy API)
                               (Proxy :: Proxy m)
                               (Proxy :: Proxy ())
                               (constDyn (BaseFullUrl Http "localhost" 8001 ""))
  bt2display "int2" getint
  bt2Table "people" getplist
  bt2Table "districts" getdistricts
  bt2Table "locations" getlocations

  ev <- bt_ev "locations" getlocations
  ld <- (holdDyn [] ev)
  configDyn <- return $ fmap mkConfig ld 
  mapWidgetDyn configDyn

--  pb <- getPostBuild >>= delay 0.1 --wait rendering
--  configDyn <- holdDyn config (pb $> config)
--  mapWidgetDyn configDyn
  return ()

-- api2dyn api =

mapWidgetDyn :: MonadWidget t m => Dynamic t (Config Int)  -> m ()
mapWidgetDyn configDyn = do
  (Element _ mapEl, _) <- elAttr' "div" ("style" =: "width: 500px; height: 600px;") blank
  maps <- googleMaps mapEl (ApiKey "AIzaSyAXn2V4wLK1QeU6YEFBFCApDQBPyajr8d0") configDyn
  mc <- mapEvent Click maps 
  return ()

mkConfig :: [Location] -> Config Int
mkConfig xs = config {
  _config_markers = fromList $ zipWith (,) [0..] (map marker xs) }
  --_config_infoWindows = fromList $ zipWith (,) [0..] (replicate 3 def)
