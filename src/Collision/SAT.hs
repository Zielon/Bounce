module Collision.SAT(
    polygonsCollision,
    polygonsCircleCollision
) where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map                      as M

import API.Ternary
import GameObjects.Objects.Polygon   as P
import GameObjects.Objects.Ball      as B
import Collision.VectorOperations    as O
import GameObjects.GameObject

-- | The polygon with polytgon collision
--
polygonsCircleCollision :: GameObject -> GameObject -> IORef (Map Int GameObject) -> IO ()
polygonsCircleCollision (GameObject a) (GameObject b) ioObjects = do
    objects <- get ioObjects
    getId a == getId b ? return () :? do
        let a_points = getPoints a
            a_edges  = (P.edgefiy a_points) ++ [(last a_points, head a_points)] 
            b_center = getCenter b
        forM_ a_edges $ \(vertex, nextVertex) -> do
            let axis = b_center -. vertex
                edge = nextVertex -. vertex
                dot  = dotProduct edge axis
                radius = getRadius b
            if (magnitude axis) - radius <= 0 
            then putStrLn $ printf "Collision outside the Voroni Regions | %d" (getId a)
            else if dot >= 0 && dot <= squered edge
                then do
                    let projection = vertex +. (edge *. (dot / squered edge))
                        center_vector = projection -. b_center
                    if magnitude center_vector <= radius
                    then putStrLn $ printf "Collision inside the Voroni Regions | %d" (getId a)
                    else putStrLn $ printf "No %d" (getId a)
            else return ()
    where squered (x,y) = x*x + y*y

-- | The polygon with polytgon collision
--
polygonsCollision :: GameObject -> GameObject -> IORef (Map Int GameObject) -> IO ()
polygonsCollision (GameObject a) (GameObject b) ioObjects = do
    intersect           <- newIORef True
    willIntersect       <- newIORef True
    translationAxis     <- newIORef (0, 0)
    intervalDistance    <- newIORef 0.0
    minIntervalDistance <- newIORef _INFINITY

    getId a == getId b ? return () :? do
        let b_edges = getEdges b
        let a_edges = getEdges a
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

    wI == True ? ioObjects $~! (\p -> M.insert id (GameObject (setOffset mtv             (setVelocity (0,0) a))) p) :? 
                 ioObjects $~! (\p -> M.insert id (GameObject (setOffset (getVelocity a) (setVelocity (0,0) a))) p)

    where _INFINITY = 999999999.9