module Main where
import Common
--import Servant.Server
import Data.Proxy
--import Network.Wai
--import Network.Wai.Handler.Warp
import Servant.API
--import           Servant
--import           Servant.Server
import           Servant.Server.Internal.SnapShims
--import           Snap
import           Snap.Core
import           Snap.Http.Server


main :: IO ()
main = run 8081 app1

int = 99

server :: Server API
server = return int

api :: Proxy API 
api = Proxy

app1 :: Application
app1 = serve api server
data App = App

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT ServantErr IO' monad.
server :: Server API '[BasicAuthCheck (Handler App App) ()] (Handler App App)
server = return int
    :<|> serveDirectory "static"

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Handler App App ()
test = serveSnapWithContext api
       (BasicAuthCheck (\_ -> return @(Handler App App) (Authorized ())) :. EmptyContext) server

initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "example" Nothing $ do
  addRoutes [("", test)
            ,("", serveDirectory "static")
            ]
  return App

-- Put this all to work!
main :: IO ()
main = serveSnaplet mempty initApp
