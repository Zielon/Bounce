import Graphics.UI.GLUT
import Graphics.UI.GLUT.Callbacks.Global
import Data.IORef
import System.IO
import Control.Concurrent
import Control.Monad
import Text.Printf

import Bindings
import Environment
import Keys
import PhysicsEngine
import FloorGenerator

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 500 500
  createWindow "Bounce"
  reshapeCallback $= Just reshape

  delta     <- newIORef 0.0
  velocityX <- newIORef 0.0
  velocityY <- newIORef 0.1
  force     <- newIORef 0
  pos       <- newIORef (0.0, 0.4)
  angle     <- newIORef 0
  floors    <- newIORef $ getFloors [(0.5, 0.5), (-0.5, -0.5)]

  setProjection
  setLights
  setMaterial

  -- Register callback
  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboardMouse force velocityX velocityY pos)
  idleCallback          $= Just (idle angle delta)
  displayCallback       $= display velocityX velocityY angle pos floors

  -- Gravity update thread
  forkIO $ forever $ do
     threadDelay 4000   -- wait 4 ms
     floors $~! (\f -> moveDown f 0.001)
     updateGravity velocityX velocityY pos 0.005 -- dt

  -- Collision detection thread
  forkIO $ forever $ do
     threadDelay 1      -- wait 1 μs
     collisionBoundaries velocityY velocityX pos
  
  forkIO $ forever $ do
     threadDelay 1      -- wait 1 μs
     collisionEdges velocityY velocityX floors pos

  -- Main OpenGL loop with callbacks
  mainLoop