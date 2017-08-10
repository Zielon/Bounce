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

display :: IORef (Map Int GameObject) -> IORef (Map Int Widget) -> DisplayCallback
display arena widgets = do
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity

  arena'    <- get arena
  widgets'  <- get widgets

  -- | The render section ----------------------
  forM_ arena'   $ \(GameObject o) -> G.draw o  -- Arena objects
  forM_ widgets' $ \(Widget w)     -> W.draw w  -- Widgets

  swapBuffers

idle :: IdleCallback
idle = do postRedisplay Nothing