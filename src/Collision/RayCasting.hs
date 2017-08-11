module Collision.RayCasting where

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
import Collision.Helpers

pointInPolygon :: GameObject -> Vector -> IORef (Map Int GameObject) -> IO ()
pointInPolygon (GameObject a) mouse ioObjects = do


pointInCircle :: GameObject -> Vector -> IORef (Map Int GameObject) -> IO ()
pointInCircle (GameObject a) mouse ioObjects = do