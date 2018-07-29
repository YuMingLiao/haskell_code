{-# Language OverloadedStrings #-}
import Prelude hiding (writeFile)
import Fake
import Types
import Fake.Provider.Person.EN_US
import Control.Monad
import Data.Csv
import Data.ByteString.Lazy (writeFile)
main = do
  x <- replicateM 100 $ generate personName
  let y = uncurry Person <$> zip x (take 100 (cycle [1..4]))
  writeFile "people.csv" $ encode (y)

