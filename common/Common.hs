{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}
module Common where
import Data.Text as T
import Servant.API
import Data.Proxy
import Reflex.Dom
import GHC.Generics
import Servant.Reflex 

type API = "getint"  :> Get '[JSON] Int
{-
      :<|> "sayhi"   :> QueryParam  "username" Text
                     :> QueryParams "greetings" Text
                     :> QueryFlag   "gusto"
                     :> Get '[JSON] Text
      :<|> "double" :> ReqBody '[JSON] Double
                    :> Post '[JSON] Double
-}
      :<|> Raw

runGUI :: forall t m. MonadWidget t m => m ()
runGUI = do
  -- servant-reflex computes FRP functions for each API endpoint
  let (getint {-:<|> sayhi :<|> doubleit-} :<|> _) = client (Proxy :: Proxy API)
                                                        (Proxy :: Proxy m)
                                                        (Proxy :: Proxy ())
                                                        (constDyn (BasePath "/"))
  return ()
