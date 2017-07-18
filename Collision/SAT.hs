module Collision.SAT where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map             as M
import Data.Set

import Collision.AABB
import Collision.Operations as O
import GameObjects.Polygon  as P

-- Segregating axis theorem
polygonCollision :: IORef (Map Int GamePolygon) -> IO ()
polygonCollision ioPolygons = do
     polygons  <- get ioPolygons
     forM_ polygons $ \a -> do
         let a_edges = getEdges a
             a_id = P.id a
         forM_ polygons $ \b -> do
             
             intersect           <- newIORef True
             willIntersect       <- newIORef True
             translationAxis     <- newIORef (0,0)
             intervalDistance    <- newIORef 0.0
             minIntervalDistance <- newIORef 9999999999.9 -- Infinity
             
             if a == b then return () else do
                let b_edges = getEdges b
                forM_ (a_edges ++ b_edges) $ \edge -> do 
                    let axis = O.normalize $ perpendicular edge
                    let (minA, maxA) = projection axis a
                    let projectionB  = projection axis b

                    if (calculateIntervalDistance (minA, maxA) projectionB) > 0 
                    then intersect $~! (\b -> False) >> return ()    -- polygons are not intersecting
                    else intersect $~! (\b -> b)

                    let projectionV = dotProduct axis (velocity a)
                    
                    if projectionV < 0 
                    then intervalDistance $~! (\i -> calculateIntervalDistance (minA + projectionV, maxA) projectionB)
                    else intervalDistance $~! (\i -> calculateIntervalDistance (minA, maxA + projectionV) projectionB)

                    iD  <- get intervalDistance
                    if iD > 0 then willIntersect $~! (\b -> False) else return ()

                    rI  <- get intersect
                    rWi <- get willIntersect

                    intervalDistance $~! (\i -> abs i)

                    if rI == True || rWi == True
                    then do
                        distance    <- get intervalDistance
                        minDistance <- get minIntervalDistance
                        if distance < minDistance then do
                            minIntervalDistance $~! (\d -> distance) >> translationAxis $~! (\a -> axis)
                            let d = (getCenter a) -. (getCenter b)
                            when ((dotProduct d axis) < 0) $ translationAxis $~! (\a -> (--.) a)
                        else return ()
                    else return ()

             if a == b then return () else do
                wI  <- get willIntersect
                ta  <- get translationAxis
                mid <- get minIntervalDistance
                
                let offset = (velocity a) +. (ta *. mid)
                if wI == True 
                then do
                    putStrLn $ printf "%d with %d offset == %s" (P.id a) (P.id b) $ show offset
                    >> ioPolygons $~! (\p -> M.insert a_id (P.setOffset offset (P.setVelocity (0,0) a)) p)
                else ioPolygons $~! (\p -> M.insert a_id (P.setOffset (velocity a) (P.setVelocity (0,0) a)) p)