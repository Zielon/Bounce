module API.Display (
  idle,
  display)
where

import Prelude                      hiding (id)
import Graphics.UI.GLUT             as G
import Graphics.UI.GLUT.Fonts
import Control.Monad
import Data.IORef
import System.IO
import Text.Printf
import Control.Concurrent
import Data.Map

import API.Environment

import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon
import GameObjects.GameObject

display :: IORef (Map Int Ball) ->
           IORef (Map Int GamePolygon) ->
           IORef GLfloat ->
           DisplayCallback
display balls polygons force = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity

  balls'    <- get balls
  polygons' <- get polygons
  force'    <- get force

  -- | Render section ----------------------

  -- | Polygons
  forM_ polygons' $ \polygon -> draw polygon

  -- | Ball
  forM_ balls' $ \ball -> draw ball

  swapBuffers
  where 
    getColor3f :: (Float, Float, Float) -> IO ()
    getColor3f (r, g, b) = color $ Color3 r g (b :: GLfloat)

    getColor3f' :: (GLfloat, GLfloat, GLfloat) -> IO ()
    getColor3f' (r, g, b) = color $ Color3 r g (b :: GLfloat)

idle :: IdleCallback
idle = do postRedisplay Nothing