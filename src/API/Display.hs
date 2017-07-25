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
import GameObjects.GameObject      as G
import Widgets.Widget              as W

display :: IORef (Map Int GameObject) ->
           IORef (Map Int Widget) ->
           IORef GLfloat ->
           DisplayCallback
display arena widgets force = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity

  arena'    <- get arena
  force'    <- get force
  widgets'  <- get widgets

  -- | Render section ----------------------

  -- | Arena objects
  forM_ arena' $ \(GameObject o) -> (G.draw o)

  -- | Widgets
  forM_ widgets' $ \(Widget w) -> W.draw w

  swapBuffers

idle :: IdleCallback
idle = do postRedisplay Nothing