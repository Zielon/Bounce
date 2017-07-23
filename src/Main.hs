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

  balls     <- newIORef $ M.fromList [(1, Ball 1 (0.9, 0.9) (0.0, 0.1) 0.05 0 0),
                                      (2, Ball 2 (0.8, 0.8) (0.0, 0.2) 0.05 0 0), 
                                      (3, Ball 3 (0.3, 0.7) (0.0, 0.3) 0.05 0 0)]

  polygons  <- newIORef $ M.fromList [(1, P.GamePolygon 1 (0,0) [(0.1, 0.2), (0.1, 0.4), (0.2, 0.4)]),
                                      (5, P.GamePolygon 5 (0,0) [(-0.8, -0.8), (-0.8, -0.7),(0.8, -0.7), (0.8, -0.8)]),
                                      (4, P.GamePolygon 4 (0,0) [(-0.5, -0.6), (-0.4, 0.4), (-0.5, 0.4)]),
                                      (3, P.GamePolygon 3 (0,0) [(0.5, -0.6), (0.4, 0.0), (0.5, 0.0)]),
                                      (2, P.GamePolygon 2 (0,0) [(-0.2, 0.6), (0.2, 0.8), (0.1, 0.6)])]

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