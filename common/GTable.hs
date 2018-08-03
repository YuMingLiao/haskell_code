{-# Language OverloadedStrings #-}
{-# Language DefaultSignatures #-}
{-# Language TypeOperators #-}
{-# Language FlexibleContexts #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveAnyClass #-}
{-# Language RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module GTable where
import Reflex.Dom
import GHC.Generics
import Data.Data
import Data.Typeable
import Data.Proxy
import Data.Generics.Text
import Data.Text hiding (map)
import Control.Monad
import Types

table :: forall a t m. (RecordText a, Show a, MonadWidget t m, Data a, Typeable a, Generic a, GSelectors (Rep a)) => [a] -> m ()
table xs = do
  el "table" $ do
      el "tr" $ do
        let cols = pack.fst <$> gselectors (Proxy :: Proxy (Rep a)) 
        forM_ cols (el "td".text)
      forM_ xs row

row :: (RecordText a, Show a, MonadWidget t m, Data a, Typeable a) => a -> m () -- (Dynamic t [()])
row x = do 
  el "tr" $ do
    forM (rtext x) (el "td" . text)
  return()


dynTable :: (RecordText a, Show a, MonadWidget t m, Data a, Typeable a, Generic a, GSelectors (Rep a)) => Text -> Event t [a] -> m (Event t ()) 
dynTable n e = do 
    lst_d <- holdDyn [] e
    tabled <- return $ fmap table lst_d
    dyn tabled



