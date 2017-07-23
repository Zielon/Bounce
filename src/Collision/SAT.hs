module Collision.SAT where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.List                     as L
import Data.Map                      as M

import API.Ternary
import Collision.VectorOperations    as O
import GameObjects.Objects.Polygon   as P

-- | Segregating axis theorem
--
polygonCollision :: (Eq a, Ord a, GameObject a) => IORef (Map Int a) -> IO ()
polygonCollision ioPolygons = do
     polygons <- get ioPolygons
     forM_ (L.map (\(k, v) -> k) $ M.toList polygons) $ \i -> do    -- Use keys from the dictionary
         forM_ polygons $ \b -> do
            polygons  <- get ioPolygons                             -- Each time we need to fetch the newest position of the Polygon A
            let (Just a) = M.lookup i polygons
                a_edges  = getEdges a

            intersect           <- newIORef True
            willIntersect       <- newIORef True
            translationAxis     <- newIORef (0,0)
            intervalDistance    <- newIORef 0.0
            minIntervalDistance <- newIORef _INFINITY

            a == b ? return () :? do
                let b_edges = getEdges b
                forM_ (a_edges ++ b_edges) $ \edge -> do 
                    let axis = O.normalize $ perpendicular edge
                        projectionB  = projection axis b
                        (minA, maxA) = projection axis a
                       
                    calculateIntervalDistance (minA, maxA) projectionB > 0 ? intersect $~! (\b -> False) :? return ()

                    let projectionV = dotProduct axis (getVelocity a)

                    if projectionV < 0 
                    then intervalDistance $~! (\i -> calculateIntervalDistance (minA + projectionV, maxA) projectionB)
                    else intervalDistance $~! (\i -> calculateIntervalDistance (minA, maxA + projectionV) projectionB)

                    id  <- get intervalDistance
                    id > 0 ? willIntersect $~! (\b -> False) :? return ()

                    isIntersect     <- get intersect
                    goingToIntersec <- get willIntersect

                    if isIntersect == True || goingToIntersec == True then do
                        intervalDistance $~! (\i -> abs i)
                        distance    <- get intervalDistance
                        minDistance <- get minIntervalDistance
                        if distance < minDistance then do
                            minIntervalDistance $~! (\d -> distance) >> translationAxis $~! (\a -> axis)
                            let d = (getCenter a) -. (getCenter b)                                  -- We ara translating the A polygon according to the vector (A-B) [which points from B to A]
                            when ((dotProduct d axis) < 0) $ translationAxis $~! (\a -> (--.) a)    -- Negative dot product [(A-B)·Axis] means that the axis and [A-B] do not point in the same direction
                        else return ()                                                              -- By negating the translation axis we change the pointing direction
                    else return ()

            wI  <- get willIntersect
            ta  <- get translationAxis
            mid <- get minIntervalDistance
            
            let mtv = (getVelocity a) +. (ta *. mid) -- The minimum translation vector is used to push the polygons appart.
            let id = getId a

            postRedisplay Nothing
            
            wI == True ? ioPolygons $~! (\p -> M.insert id (setOffset mtv             (setVelocity (0,0) a)) p) :? 
                         ioPolygons $~! (\p -> M.insert id (setOffset (getVelocity a) (setVelocity (0,0) a)) p)

    where _INFINITY = 999999999.9