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

import GameObjects.Objects.Ball    as B
import GameObjects.Objects.Polygon as P
import GameObjects.GameObject
import Widgets.Widget              as W

display :: IORef (Map Int Ball) ->
           IORef (Map Int GamePolygon) ->
           IORef (Map Int Widget) ->
           IORef GLfloat ->
           DisplayCallback
display balls polygons widgets force = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity

  balls'    <- get balls
  polygons' <- get polygons
  force'    <- get force
  widgets'  <- get widgets

  -- | Render section ----------------------

  -- | Polygons
  forM_ polygons' $ \polygon -> P.draw polygon

  -- | Ball
  forM_ balls' $ \ball -> B.draw ball

  -- | Widgets
  forM_ widgets' $ \(Widget w) -> W.draw w

  swapBuffers

idle :: IdleCallback
idle = do postRedisplay Nothing