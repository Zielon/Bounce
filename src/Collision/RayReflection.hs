module Collision.RayReflection where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Ratio
import Data.Maybe
import Data.Map                      as M
import Data.List                     as L
import Control.Parallel.Strategies

import Factory.Producer
import API.Ternary
import GameObjects.Objects.Polygon   as P
import GameObjects.Objects.Ball      as B
import GameObjects.Objects.Segment   as S
import Collision.VectorOperations    as O
import GameObjects.GameObject
import Collision.Helpers


rayReflection :: IORef (Map Int GameObject) -> IORef [Segment] -> IORef Vector -> IO ()
rayReflection ioObjects reflections mouse = do
    m <- get mouse
    r <- get reflections

    return ()