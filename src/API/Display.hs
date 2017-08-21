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
import API.Ternary
import Factory.Producer
import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon
import Widgets.Settings
import GameObjects.GameObject      as G
import Widgets.Widget              as W
import GameObjects.Objects.Segment as S

display :: IORef (Map Int GameObject) -> IORef (Map Int Widget) -> IORef [Segment] -> DisplayCallback
display arena widgets rays = do
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity

  arena'   <- get arena
  widgets' <- get widgets
  rays'    <- get rays

  -- Check for RayCast setting is ON
  case Data.Map.lookup 3 widgets' of
    Nothing         -> return ()
    Just (Widget w) -> do
      let (Just on) = Data.Map.lookup RayCast $ getOptions w
      on == True ? forM_ rays' (\r -> S.draw r) :? return ()

  -- | The render section ----------------------
  forM_ arena'   $ \(GameObject o) -> G.draw o  -- Arena objects
  forM_ widgets' $ \(Widget w)     -> W.draw w  -- Widgets

  swapBuffers

idle :: IdleCallback
idle = do postRedisplay Nothing