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
import API.Ternary

import Collision.PhysicsEngine
import Collision.Loop

import Widgets.Widget

import Common.Drawable

import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon as P
import Factory.Producer            as Factory

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 800 800
  createWindow "Bounce"
  reshapeCallback $= Just reshape

  force    <- newIORef 0
  keys     <- newIORef getKeys
  widgets  <- newIORef getWidgetsMap
  arena    <- newIORef getArenaObjectsMap

  -- Register callbacks
  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboardMouse keys arena)
  idleCallback          $= Just (idle)
  displayCallback       $= display arena widgets force

  -- Global handler for StdGen
  generator <- newIORef (mkStdGen 0)

  -- ===== THREAD SECTION =====

  -- Gravity update and rand new floors thread
  forkIO $ forever $ do
     threadDelay 4000
  --   updateGravity arena 0.009 -- dt
     updateKeysBindings keys force arena
  --   arena $~! \p -> M.map (\(GameObject v) -> getType v /= BallType ? (GameObject (setOffset (0, -0.00005) v)) :? (GameObject v)) p

  forkIO $ forever $ do
     threadDelay 10
   --  collisionBoundaries arena
     collisionLoop arena

  -- Main OpenGL loop with callbacks
  mainLoop