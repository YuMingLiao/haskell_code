{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
--  {-# Language AllowAmbiguousTypes #-}
import Reflex.Dom
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

--import Servant.Common.BaseUrl
main :: IO ()
main = mainWidget $ do
  demo
  display =<< count =<< button "ClickMe"
  mapWidget config

demo :: forall t m. MonadWidget t m => m ()
demo = do
  let (getint :<|> getplist :<|> _) = 
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
    --   el "tr" $ el "td" $ dynText (T.pack.show <$> pListDyn)
      simpleList pListDyn displayPersonRow          -- render all station records

    return ()

-- | Create the HTML element for a single HTML table row
displayPersonRow :: MonadWidget t m => Dynamic t Person -> m ()--(Event t T.Text)
displayPersonRow dynPer =  el "tr" $ do
  el "td" $ dynText $ (^.name) <$> dynPer
  el "td" $ display $ (^.location) <$> dynPer




