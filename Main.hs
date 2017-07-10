import Graphics.UI.GLUT
import Graphics.UI.GLUT.Callbacks.Global
import Data.IORef
import System.IO
import Control.Concurrent
import Control.Monad
import Text.Printf
import System.Random

import API.Bindings
import API.Keys
import API.CollisionTests
import Engines.PhysicsEngine
import Engines.FloorEngine
import GameObjects.Ball

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 600 600
  createWindow "Bounce"
  reshapeCallback $= Just reshape

  ball      <- newIORef $ Ball 0.0 0.7 0.0 0.1 0 0
  delta     <- newIORef 0.0
  angle     <- newIORef 0
  force     <- newIORef 0
  floors    <- newIORef $ getFloors getMockedFloors
  
  -- Register callbacks
  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboardMouse force ball)
  idleCallback          $= Just (idle angle delta)
  displayCallback       $= display ball angle floors force

  -- Global handler for StdGen
  generator <- newIORef (mkStdGen 0)

  gridIntersect2D floors

  -- Gravity update and rand new floors thread
  forkIO $ forever $ do
     threadDelay 4000   -- wait 4 ms
     moveDownAll 0.00005 generator floors
     gridIntersect2D floors
     updateGravity ball 0.005 -- dt

  -- Collision detection thread
  forkIO $ forever $ do
     threadDelay 1      -- wait 1 μs
     collisionBoundaries ball
  
  forkIO $ forever $ do
     threadDelay 1      -- wait 1 μs
     collisionEdges ball floors

  -- Main OpenGL loop with callbacks
  mainLoop