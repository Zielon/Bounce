module Collision.Loop where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.List                     as L
import Data.Map                      as M

import API.Ternary
import Collision.VectorOperations    as O
import Collision.SAT
import Collision.RayCasting
import GameObjects.Objects.Polygon   as P
import GameObjects.Objects.Ball      as B
import GameObjects.GameObject

-- | Segregating axis theorem for arena's objects
--
collisionLoop :: IORef (Map Int GameObject) -> IO ()
collisionLoop ioObjects = do
     objects       <- get ioObjects
     let ids = (L.map (\(k, v) -> k) $ M.toList objects)
     forM_ ids $ \i -> do                  -- Use keys from the dictionary
         forM_ ids $ \j -> do
            i == j ? return () :? do
                case M.lookup i objects of
                    Nothing             -> return ()
                    Just (GameObject a) -> do
                        case M.lookup j objects of
                            Nothing             -> return ()
                            Just (GameObject b) -> do
                                let iA = getType a
                                    jB = getType b

                                -- Choose the right collision test according to the object type
                                if iA == PolygonType && jB == PolygonType   then do
                                    let (i1, j1) = isHovered b
                                    polygonsCollision i1 j1 ioObjects
                                else if jB == PolygonType && iA == BallType then polygonsCircleCollision j i ioObjects
                                else if iA == PolygonType && jB == BallType then polygonsCircleCollision i j ioObjects
                                else do 
                                    let (i1, j1) = isHovered b
                                    circlesCollision i1 j1 ioObjects
                        where isHovered b = getHovered b == True ? (j, i) :? (i, j)

-- | Check is the mouse position is inside an object
--
pointInObjects :: IORef (Map Int GameObject) -> IORef Vector -> IO ()
pointInObjects ioObjects mouse = do
     objects       <- get ioObjects
     mousePosition <- get mouse
     let ids = (L.map (\(k, v) -> k) $ M.toList objects)
     forM_ ids $ \i -> do                  -- Use keys from the dictionary
     case M.lookup i objects of
        Nothing             -> return ()
        Just (GameObject a) -> do
            let iA = getType a
            if iA == PolygonType   then pointInPolygon i mousePosition ioObjects
            else if iA == BallType then pointInCircle  i mousePosition ioObjects
            else return ()