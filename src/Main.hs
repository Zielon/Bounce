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
import Collision.RayCasting
import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon as P
import Factory.Producer            as Factory

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered, WithSamplesPerPixel 5]
  initialWindowSize $= Size 800 800
  createWindow "Bounce"

  keys          <- newIORef getKeys
  buttons       <- newIORef getButtons
  widgets       <- newIORef getWidgetsMap
  arena         <- newIORef getArenaObjectsMap
  rays          <- newIORef $ getSegments 500 (0, 0)
  size          <- newIORef $ Size 800 800
  mousePosition <- newIORef (0, 0)

  -- Register callbacks
  reshapeCallback       $= Just (reshape size)
  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboard keys arena)
  passiveMotionCallback $= Just (mouseMotion mousePosition size)
  motionCallback        $= Just (moveObject arena mousePosition size)
  idleCallback          $= Just (idle)
  displayCallback       $= display arena widgets rays
  polygonSmooth         $= Enabled

  -- ===== THREAD SECTION =====

  -- Gravity update and rand new floors thread
  forkIO $ forever $ do
     threadDelay 1000
     -- updateGravity arena 0.0009 -- dt
     updateKeysBindings keys arena widgets
     pointInObjects arena mousePosition

  forkIO $ forever $ do
    threadDelay 1000
    rayCasting arena rays mousePosition

  forkIO $ forever $ do
     threadDelay 1000
     collisionBoundaries arena
     collisionLoop arena

  -- Main OpenGL loop with callbacks
  mainLoop