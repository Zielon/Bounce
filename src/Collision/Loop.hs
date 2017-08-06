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
import GameObjects.Objects.Polygon   as P
import GameObjects.Objects.Ball      as B
import GameObjects.GameObject

-- | Segregating axis theorem for arena's objects
--
collisionLoop :: IORef (Map Int GameObject) -> IO ()
collisionLoop ioObjects = do
     objects <- get ioObjects
     let ids = (L.map (\(k, v) -> k) $ M.toList objects)
     forM_ ids $ \i -> do                  -- Use keys from the dictionary
         forM_ ids $ \j -> do
            i == j ? return () :? do
                objects  <- get ioObjects  -- Each time we need to fetch the newest positions
                case M.lookup i objects of
                    Nothing             -> return ()
                    Just (GameObject a) -> do
                        case M.lookup j objects of
                            Nothing             -> return ()
                            Just (GameObject b) -> do
                                let typeA = getType a
                                    typeB = getType b
                                -- Choose the right collision test according to the object type
                                if typeA == PolygonType && typeB == PolygonType   then polygonsCollision (GameObject a) (GameObject b) ioObjects
                                else if typeB == PolygonType && typeA == BallType then polygonsCircleCollision (GameObject b) (GameObject a) ioObjects
                                else if typeA == PolygonType && typeB == BallType then polygonsCircleCollision (GameObject a) (GameObject b) ioObjects
                                else circlesCollision (GameObject a) (GameObject b) ioObjects