{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}
{-# Language TypeApplications #-}
module Main where
import Common
import Data.Proxy
--import Network.Wai
--import Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Server
import           Servant.Server.Internal.SnapShims
import           Snap
import           Snap.Core
import           Snap.Http.Server
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Util.CORS
import           Snap.Http.Server
import           Servant
import Control.Monad.IO.Class
import CSVParser

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT ServantErr IO' monad.
server :: Server API '[BasicAuthCheck (Handler App App) ()] (Handler App App)
server = return 99
    :<|> list 
    :<|> listDistricts
    :<|> listLocations
    :<|> serveDirectory "static"
  where
    list = do
      ppl <- liftIO $ getPeople "./data/people.csv"
      return ppl
    listDistricts = do
      return =<< liftIO $ getDistricts "./data/district.csv"
    listLocations = do
      return =<< liftIO $ getLocations "./data/locations_gps.csv"

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Handler App App ()
test = serveSnapWithContext api
         (BasicAuthCheck (\_ -> return @(Handler App App) (Authorized ())) :. EmptyContext) server

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "example" Nothing $ do
  wrapSite $ applyCORS defaultOptions site
  return App

site = 
  addRoutes [("", test)
            ,("", serveDirectory "static")
            ]

main :: IO ()
main = serveSnaplet mempty initApp

data App = App

api :: Proxy API 
api = Proxy
