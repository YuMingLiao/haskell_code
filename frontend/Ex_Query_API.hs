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

    return ()



