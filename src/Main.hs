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

import Collision.AABB
import Collision.PhysicsEngine
import Collision.SAT

import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon as P
import GameObjects.GameEnum        as Type
import Factory.Producer            as Factory

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 600 600
  createWindow "Bounce"
  reshapeCallback $= Just reshape

  delta     <- newIORef 0.0
  angle     <- newIORef 0
  force     <- newIORef 0
  keys      <- newIORef getKeys
  balls     <- newIORef $ M.fromList $ Factory.createBallObjects
  polygons  <- newIORef $ M.fromList $ Factory.createPolygonObjects

  -- Register callbacks
  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboardMouse keys polygons)
  idleCallback          $= Just (idle)
  displayCallback       $= display balls polygons force

  -- Global handler for StdGen
  generator <- newIORef (mkStdGen 0)

  -- ===== THREAD SECTION =====

  -- Gravity update and rand new floors thread
  forkIO $ forever $ do
     threadDelay 4000
     updateGravity balls 0.009 -- dt
     updateKeysBindings keys force balls
     polygons $~! \p -> M.map (\v-> (P.id v) /= 5 ? setOffset (0, -0.00005) v :? v) p

  forkIO $ forever $ do
     threadDelay 10
     collisionBoundaries balls
     polygonCollision polygons

  -- Main OpenGL loop with callbacks
  mainLoop