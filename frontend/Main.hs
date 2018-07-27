{-# LANGUAGE OverloadedStrings #-}
import Reflex.Dom
import MapWidget

main :: IO ()
main = mainWidget $ do
  display =<< count =<< button "ClickMe"
  mapWidget config
