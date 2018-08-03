module TypeClass where
import Types
import GoogleMapsReflex.JSTypes.Marker
import GoogleMapsReflex.JSTypes.LatLng
import Control.Lens
import GoogleMapsReflex
import qualified Data.Text as T
import Reflex.Dom (run)
import Reflex.Dom.Core hiding (Click,Drop)
import GHCJS.DOM.Types (JSM)
import qualified Data.Text as T
import Data.Default
import Data.Map (fromList)
import qualified Data.Text as T

--Marker
class Marker a where
  marker :: a -> MarkerOptions

instance Marker District where
  marker d = marker $ LatLng (d^.lat) (d^.lng)

instance Marker Location where
  marker l = marker $ LatLng (l^.lat) (l^.lng)

instance Marker LatLng where
  marker ll = def { _markerOptions_position = ll }

instance Default LatLng where
  def = LatLng 23.9756500 120.97388194

instance Default InfoWindowOptions where
  def = InfoWindowOptions {
          _infoWindowOptions_content = ContentText (T.pack "def"),
          _infoWindowOptions_disableAutoPan = True,
          _infoWindowOptions_maxWidth = 100,
          _infoWindowOptions_pixelOffset = def,
          _infoWindowOptions_position = def,
          _infoWindowOptions_zIndex = 0
        }

instance Default InfoWindowState where
  def = InfoWindowState {
          _infoWindowState_options = def,
          _infoWindowState_open = True
        }

instance Default Size where
  def = Size { 
          _size_width = 100,
          _size_height = 20,
          _size_widthUnit = Nothing,
          _size_heightUnit = Nothing
        }

defMapOption = def {
             _mapOptions_center = def,
             _mapOptions_zoom   = 8,
             _mapOptions_zoomControl = False 
}

config :: Config Int
config = def {
       _config_mapOptions = defMapOption,
       _config_markers = fromList [ (0, def { _markerOptions_position = def } ) ] 
}

index xs = fromList $ zipWith (,) [0..] xs

