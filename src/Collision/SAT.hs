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
import Data.List                     as L

import API.Ternary
import GameObjects.Objects.Polygon   as P
import GameObjects.Objects.Ball      as B
import Collision.VectorOperations    as O
import GameObjects.GameObject
import Collision.Helpers


-- | A ball with a ball collison
--
circlesCollision :: Int -> Int -> IORef (Map Int GameObject) -> IO ()
circlesCollision i j ioObjects = do
    objects <- get ioObjects
    case M.lookup i objects of
        Nothing             -> return ()
        Just (GameObject a) -> do
            case M.lookup j objects of
                Nothing             -> return ()
                Just (GameObject b) -> do
                    let a_radius = getRadius a
                        a_center = getCenter a
                        b_radius = getRadius b
                        b_center = getCenter b
                        axis = b_center -. a_center
                        result = magnitude axis <= b_radius + a_radius

                    changePosition ioObjects (O.normalize axis) (abs(magnitude axis - b_radius - a_radius)) result (GameObject a) (GameObject b)

-- | A polygon with a ball collision
--
polygonsCircleCollision :: Int -> Int -> IORef (Map Int GameObject) -> IO ()
polygonsCircleCollision i j ioObjects = do

    objects             <- get ioObjects
    intersect           <- newIORef False
    translationAxis     <- newIORef (0.0, 0.0)
    projectedVector     <- newIORef (0.0, 0.0)
    minIntervalDistance <- newIORef infinity

    case M.lookup i objects of
        Nothing             -> return ()
        Just (GameObject a) -> do
            case M.lookup j objects of
                Nothing             -> return ()
                Just (GameObject b) -> do
                    let a_points = getPoints a
                        a_edges  = (P.edgefiy a_points) ++ [(last a_points, head a_points)]
                        b_center = getCenter b
                        radius   = getRadius b

                    forM_ a_edges $ \(start, end) -> do
                        let axis   = b_center -. start
                            edge   = end -. start
                            length = magnitude edge
                            dot    = axis • (O.normalize edge)

                    -- Check for a collision outside the Voroni Regions
                        if dot <= 0.0 then do projectedVector ^& (\v -> start)
                        else if dot >= length then do projectedVector ^& (\v -> end)
                        else do 
                            let normal = (O.normalize edge) *. dot
                                vector = normal +. start
                            projectedVector ^& (\v -> vector)

                        minDistance <- get minIntervalDistance
                        projection  <- get projectedVector

                        let centerVector = b_center -. projection
                            distance     = magnitude centerVector

                        distance <= radius && distance < minDistance ? minIntervalDistance ^& (\d -> distance)
                                >> translationAxis ^& (\a -> O.normalize centerVector)
                                >> intersect ^& (\b -> True) :? return ()

                    inter  <- get intersect
                    ta     <- get translationAxis
                    mid    <- get minIntervalDistance

                    changePosition ioObjects ta (abs(radius - mid)) inter (GameObject a) (GameObject b)

    where squered (x,y) = x*x + y*y

-- | A polygon with a polygon collision
--
polygonsCollision :: Int -> Int -> IORef (Map Int GameObject) -> IO ()
polygonsCollision i j ioObjects = do
    intersect           <- newIORef True
    willIntersect       <- newIORef True
    translationAxis     <- newIORef (0, 0)
    intervalDistance    <- newIORef 0.0
    minIntervalDistance <- newIORef infinity
    objects <- get ioObjects

    case M.lookup i objects of
        Nothing             -> return ()
        Just (GameObject a) -> do
            case M.lookup j objects of
                Nothing             -> return ()
                Just (GameObject b) -> do
                    let b_edges = getEdges b
                    let a_edges = getEdges a
                    forM_ (a_edges ++ b_edges) $ \edge -> do 
                        let axis = O.normalize $ perpendicular edge
                            projectionB  = projection axis b
                            (minA, maxA) = projection axis a
                        
                        calculateIntervalDistance (minA, maxA) projectionB > 0 ? intersect ^& (\b -> False) :? return ()

                        let projectionV = axis • (getVelocity a)

                        if projectionV < 0 
                        then intervalDistance ^& (\i -> calculateIntervalDistance (minA + projectionV, maxA) projectionB)
                        else intervalDistance ^& (\i -> calculateIntervalDistance (minA, maxA + projectionV) projectionB)

                        id  <- get intervalDistance
                        id > 0 ? willIntersect ^& (\b -> False) :? return ()

                        isIntersect     <- get intersect
                        goingToIntersec <- get willIntersect

                        if isIntersect == True || goingToIntersec == True then do
                            intervalDistance ^& (\i -> abs i)
                            distance    <- get intervalDistance
                            minDistance <- get minIntervalDistance
                            if distance < minDistance then do
                                minIntervalDistance ^& (\d -> distance) >> translationAxis ^& (\a -> axis)
                                let d = (getCenter a) -. (getCenter b)                         -- We ara translating the A polygon according to the vector (A-B) [which points from B to A]
                                when ((d • axis) < 0) $ translationAxis ^& (\a -> (--.) a)     -- Negative dot product [(A-B)·Axis] means that the axis and [A-B] do not point in the same direction
                            else return ()                                                     -- By negating the translation axis we change the pointing direction
                        else return ()

                    inter  <- get willIntersect
                    ta  <- get translationAxis
                    mid <- get minIntervalDistance

                    changePosition ioObjects ta mid inter (GameObject b) (GameObject a)

-- ========== PRIVATE SECTION ==========

changePosition :: IORef (Map Int GameObject) -> Vector -> GLfloat -> Bool -> GameObject -> GameObject -> IO ()
changePosition ioObjects translationAxis lenght inter (GameObject a) (GameObject b) = do
    objects <- get ioObjects
    let value = L.find (\(k, (GameObject v)) -> getHovered v == True ) $ toList objects
    case value of
        Nothing                  -> return ()
        Just (k, (GameObject v)) -> do
            let lengthToA = O.lenght (getCenter v) (getCenter a)
                lengthToB = O.lenght (getCenter v) (getCenter b)
            if lengthToA >= lengthToB then do
                let velocity = getVelocity a
                    mtv = velocity -. (translationAxis *. lenght)
                inter == True ? ioObjects ^& (\p -> (GameObject $ setOffset mtv  a) #- p) :?  ioObjects ^& (\p -> (GameObject $ setOffset velocity a) #- p)
            else do
                let velocity = getVelocity b
                    mtv = velocity +. (translationAxis *. lenght)
                inter == True ? ioObjects ^& (\p -> (GameObject $ setOffset mtv  b) #- p) :?  ioObjects ^& (\p -> (GameObject $ setOffset velocity b) #- p)