module Collision.SAT where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map as M
import Data.Set as S

import Collision.AABB
import Collision.Helpers

import GameObjects.Polygon

-- Segregating axis theorem
polygonCollision :: IORef (Map Int GamePolygon) -> IO ()
polygonCollision ioPolygons = do
     polygons <- get ioPolygons
     forM_ polygons $ \a ->
         forM_ polygons $ \b ->
             if a == b then return ()
             else return ()
                  --let axis = perpendicular 
