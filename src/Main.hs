import Graphics.UI.GLUT
import Graphics.UI.GLUT.Callbacks.Global
import Data.IORef
import System.IO
import Control.Concurrent
import Control.Monad
import Text.Printf
import System.Random
import Data.List
import Data.Map as M

import API.Display
import API.Bindings
import API.Keys
import API.Buttons
import API.Ternary
import API.Serialize

import Collision.PhysicsEngine
import Collision.Loop

import Widgets.Widget

import Common.Drawable

import Collision.Helpers

import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon as P
import Factory.Producer            as Factory

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, WithSamplesPerPixel 5]
  initialWindowSize $= Size 800 800
  createWindow "Bounce"
  reshapeCallback $= Just reshape

  keys         <- newIORef getKeys
  buttons      <- newIORef getButtons
  widgets      <- newIORef getWidgetsMap
  arena        <- newIORef getArenaObjectsMap
  mousePosition<- newIORef (0.0, 0.0)

  -- Register callbacks
  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboard keys arena)
  passiveMotionCallback $= Just (mouseMotion mousePosition)
  motionCallback        $= Just (moveObject arena mousePosition)
  idleCallback          $= Just (idle)
  displayCallback       $= display arena widgets
  polygonSmooth         $= Enabled

  -- Global handler for StdGen
  generator <- newIORef (mkStdGen 0)

  -- serialize "ball.json"

  -- ===== THREAD SECTION =====

  -- Gravity update and rand new floors thread
  forkIO $ forever $ do
     threadDelay 100
     -- updateGravity arena 0.0009 -- dt
     updateKeysBindings keys arena widgets
     pointInObjects arena mousePosition

  forkIO $ forever $ do
     threadDelay 100
     collisionBoundaries arena
     collisionLoop arena

  -- Main OpenGL loop with callbacks
  mainLoop