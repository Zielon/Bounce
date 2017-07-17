module Collision.SAT where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map
import Data.Set

import Collision.AABB
import Collision.Operations as O
import GameObjects.Polygon  as P

-- Segregating axis theorem
polygonCollision :: IORef (Map Int GamePolygon) -> IO ()
polygonCollision ioPolygons = do
     polygons  <- get ioPolygons
     forM_ polygons $ \a -> do
         intersect <- newIORef True
         let a_edges = getEdges a
         forM_ polygons $ \b ->
             if a == b then return () else do
                let b_edges = getEdges b
                forM_ (a_edges ++ b_edges) $ \edge -> do 
                    let axis = O.normalize $ perpendicular edge
                    let projectionA = projection axis a
                    let projectionB = projection axis b

                    if (intervalDistance projectionA projectionB) > 0 
                    then intersect $~! (\b -> False) >> return ()    -- polygons are not intersecting
                    else intersect $~! (\b -> b)

                -- Print result
                bool <- get intersect
                if bool == True then putStrLn $ printf "Intersection A %d with B %d == %s" (P.id a) (P.id b) (show bool)
                else return ()