module GameArea.Manager where

import Graphics.UI.GLUT
import Data.IORef
import Data.Map

clearScene :: IORef (Map Int GamePolygon) -> IO ()
clearScene polygons = return ()