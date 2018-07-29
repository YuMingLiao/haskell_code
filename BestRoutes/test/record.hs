import Data.List

data RecA = RecA {
    a :: Int
  , b :: String
  , c :: Float
} deriving Show

test1 = RecA 3 "asdffas" 2.5

main = putStrLn $ show $ RL $ replicate 3 test1

newtype RecordList = RL [RecA]

instance Show RecordList where
    show (RL xs) =  unlines (map show xs)
