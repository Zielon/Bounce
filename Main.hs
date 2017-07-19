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

import Map.FloorEngine

import GameObjects.Ball
import GameObjects.Polygon as P

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 600 600
  createWindow "Bounce"
  reshapeCallback $= Just reshape

  ball      <- newIORef $ Ball 0.9 0.9 0.0 0.1 0.05 0 0
  delta     <- newIORef 0.0
  angle     <- newIORef 0
  force     <- newIORef 0
  keys      <- newIORef getKeys
  floors    <- newIORef $ getFloors $ [] -- getMockedFloors
  polygons  <- newIORef $ M.fromList [(1, P.GamePolygon 1 (0,0) [(0.1, 0.2), (0.1, 0.4), (0.2, 0.4)]),
                                      (5, P.GamePolygon 5 (0,0) [(-0.8, -0.8), (-0.6, -0.7), (0.8, -0.8), (0.8, -0.7)]),
                                      (4, P.GamePolygon 4 (0,0) [(-0.5, -0.6), (-0.4, 0.4), (-0.5, 0.4)]),
                                      (3, P.GamePolygon 3 (0,0) [(0.5, -0.6), (0.4, 0.0), (0.5, 0.0)]),
                                      (2, P.GamePolygon 2 (0,0) [(-0.2, 0.6), (0.2, 0.8), (0.1, 0.6)])]

  -- Register callbacks
  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboardMouse keys polygons)
  idleCallback          $= Just (idle angle delta)
  displayCallback       $= display ball angle floors polygons force

  -- Global handler for StdGen
  generator <- newIORef (mkStdGen 0)

  -- Gravity update and rand new floors thread
  forkIO $ forever $ do
     threadDelay 5000   -- wait 5 ms
     moveDownAll 0.00005 generator floors
     updateGravity ball 0.009 -- dt
     updateKeysBindings keys force ball
     polygons $~! \p -> M.map (\v-> (P.id v) /= 5 ? setOffset (0, -0.0005) v :? v) p 

  forkIO $ forever $ do
     threadDelay 10
     collisionBoundaries ball
     collisionEdges ball floors
     polygonCollision polygons

  forkIO $ forever $ do
     threadDelay 10
     gridIntersect2D floors

  -- Main OpenGL loop with callbacks
  mainLoop