module Collision.SAT where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map as M
import Data.Set as S

import Collision.AABB
import GameObjects.Polygon

-- Find the axis perpendicular to the current edge
polygonCollision :: IORef (Map Int Polygon) -> IO ()
polygonCollision ioPolygons = do
     polygons <- get ioPolygons


     return ()
