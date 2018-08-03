{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ShowText where
import Data.Proxy
import Data.Text

--type level function
type family (F a) :: Bool where
  F Text  = 'True
  F String  = 'True
  F a     = 'False

class ShowText a where
  showtext :: a -> Text

instance (F a ~ flag, ShowText' flag a) => ShowText a where
  showtext = showtext' (Proxy :: Proxy flag)

class ShowText' (flag :: Bool) a where
  showtext' :: Proxy flag -> a -> Text

instance ShowText' 'True Text where
  showtext' _ x = x

instance ShowText' 'True String where
  showtext' _ x = pack x

instance (Show a) => ShowText' 'False a where
  showtext' _ x = pack (show x)
