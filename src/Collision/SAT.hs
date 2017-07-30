module Collision.SAT(
    polygonsCollision,
    circlesCollision,
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

-- | A ball with a ball collison
--
circlesCollision :: GameObject -> GameObject -> IORef (Map Int GameObject) -> IO ()
circlesCollision (GameObject a) (GameObject b) ioObjects = do
    getId a == getId b ? return () :? do
        let a_radius = getRadius a
            a_center = getCenter a
            b_radius = getRadius b
            b_center = getCenter b
            axis = b_center -. a_center
            mtv = (getVelocity b) +. ((O.normalize axis) *. abs(magnitude axis - b_radius - a_radius))  -- The minimum translation vector.
        if magnitude axis <= b_radius + a_radius
        then ioObjects $~! (\p -> M.insert (getId b) (GameObject (setOffset mtv b)) p)
        else ioObjects $~! (\p -> M.insert (getId b) (GameObject (setOffset (getVelocity b) (setVelocity (0,0) b))) p)

-- | A polygon with a ball collision
--
polygonsCircleCollision :: GameObject -> GameObject -> IORef (Map Int GameObject) -> IO ()
polygonsCircleCollision (GameObject a) (GameObject b) ioObjects = do

    intersect           <- newIORef False
    translationAxis     <- newIORef (0, 0)
    minIntervalDistance <- newIORef _INFINITY

    getId a == getId b ? return () :? do
        let a_points = getPoints a
            a_edges  = (P.edgefiy a_points) ++ [(last a_points, head a_points)]
            b_center = getCenter b
            radius = getRadius b

        -- Check for a collision inside the Voroni Regions
        forM_ a_edges $ \(vertex, nextVertex) -> do
            i <- get intersect
            i == True ? return () :? do
                let axis = b_center -. vertex
                    edge = nextVertex -. vertex
                    dot  = dotProduct edge axis
                if dot >= 0 && dot <= squered edge && squered edge /= 0 then do
                    let projection = vertex +. (edge *. (dot / squered edge))
                        center_vector = projection -. b_center
                    magnitude center_vector > radius ? return () :? do
                        minDistance <- get minIntervalDistance
                        let distance = abs(radius - magnitude center_vector)
                        intersect $~! (\b -> True)
                        distance < minDistance ? minIntervalDistance $~! (\d -> distance) >> translationAxis $~! (\a -> O.normalize center_vector) :? return ()
                else return ()

        -- Check for a collision outside the Voroni Regions
        forM_ a_edges $ \(vertex, nextVertex) -> do
            i <- get intersect
            i == True ? return () :? do
                let axis = b_center -. vertex
                    edge = nextVertex -. vertex
                    dot  = dotProduct edge axis
                magnitude axis - radius > 0 ? return () :? do
                    minDistance <- get minIntervalDistance
                    let distance = magnitude axis - radius
                    intersect $~! (\b -> True)
                    distance < minDistance ? minIntervalDistance $~! (\d -> distance) >> translationAxis $~! (\a -> O.normalize axis) :? return ()

    wI  <- get intersect
    ta  <- get translationAxis
    mid <- get minIntervalDistance
    
    let mtv = (getVelocity a) +. (ta *. mid) -- The minimum translation vector.
    let id = getId a

    wI == True ? ioObjects $~! (\p -> M.insert id (GameObject (setOffset mtv             (setVelocity (0,0) a))) p) :? 
                 ioObjects $~! (\p -> M.insert id (GameObject (setOffset (getVelocity a) (setVelocity (0,0) a))) p)

    where _INFINITY = 999999999.9 
          squered (x,y) = x*x + y*y

-- | A polygon with a polygon collision
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