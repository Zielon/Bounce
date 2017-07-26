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

-- | Segregating axis theorem for objects
--
collisionLoop :: IORef (Map Int GameObject) -> IO ()
collisionLoop ioObjects = do
     objects <- get ioObjects
     forM_ (L.map (\(k, v) -> k) $ M.toList objects) $ \i -> do    -- Use keys from the dictionary
         forM_ objects $ \(GameObject b) -> do
            objects  <- get ioObjects                              -- Each time we need to fetch the newest position of the Polygon A
            case M.lookup i objects of
                Nothing             -> return ()
                Just (GameObject a) -> do
                    let id = getId a
                        typeA = getType a
                        typeB = getType b

                    if typeA == PolygonType && typeB == PolygonType 
                    then polygonsCollision       (GameObject a) (GameObject b) ioObjects
                    else polygonsCircleCollision (GameObject a) (GameObject b) ioObjects