import Data.Proxy
import Data.Text
import Data.Data

--failed
showtextd :: a -> Text
showtextd x
  | typeOf x == typeRep (Proxy :: Proxy Text) = (x :: Text)
  | typeOf x == typeRep (Proxy :: Proxy String) = pack (x `asTypeOf` "SAdfsa")
  | otherwise = pack . show $ x

