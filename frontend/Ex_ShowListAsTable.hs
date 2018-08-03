{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
--  {-# Language AllowAmbiguousTypes #-}
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

--import Servant.Common.BaseUrl
main :: IO ()
main = mainWidget $ do
  demo
  mapWithConfigDyn

demo :: forall t m. MonadWidget t m => m ()
demo = do
  let (getint :<|> getplist :<|> getdistricts :<|> _) = 
                               client (Proxy :: Proxy API)
                               (Proxy :: Proxy m)
                               (Proxy :: Proxy ())
                               (constDyn (BaseFullUrl Http "localhost" 8001 ""))
  elClass "div" "int-demo" $ do
    intButton  <- button "Get Int"
    serverInts <- fmapMaybe reqSuccess <$> getint intButton
    display =<< holdDyn 0 serverInts

  elClass "div" "plist-demo" $ do
    plistButton  <- button "Get People List"
    serverPList <- fmapMaybe reqSuccess <$> getplist plistButton
    pListDyn <- holdDyn [] serverPList

    el "table" $ do
      el "tr" $ do
        el "td" $ text "name"
        el "td" $ text "location"
      simpleList pListDyn displayPersonRow

  elClass "div" "districts-demo" $ do
    btn <- button "Get Districts"
    serverDList <- fmapMaybe reqSuccess <$> getdistricts btn 
    dListDyn <- holdDyn [] serverDList

    el "table" $ do
      el "tr" $ do
        el "td" $ text "name"
        el "td" $ text "code"
        el "td" $ text "lag"
        el "td" $ text "lng"
      simpleList dListDyn displayDistrictRow

    return ()

displayPersonRow :: MonadWidget t m => Dynamic t Person -> m ()--(Event t T.Text)
displayPersonRow dynPer = do 
  el "tr" $ do
    el "td" $ dynText $ (^.name) <$> dynPer
    el "td" $ display $ (^.location) <$> dynPer

displayDistrictRow :: MonadWidget t m => Dynamic t District -> m ()--(Event t T.Text)
displayDistrictRow dynDis = do 
  el "tr" $ do
    el "td" $ dynText $ (^.name) <$> dynDis
    el "td" $ dynText $ (^.code) <$> dynDis
    el "td" $ display $ (^.lat) <$> dynDis
    el "td" $ display $ (^.lng) <$> dynDis





