import Conduit
import Prelude hiding (foldl1)
import Data.Conduit.Combinators hiding (print)
import TestPermute
import Group
main :: IO ()
main = print $ runConduitPure $ unfold unfoldGroup (Just $ group 5 [3,1]) .| foldl1 (\x y->x) 
