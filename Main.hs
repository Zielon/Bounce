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
import Gravity
import MapGenerator

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 500 500
  createWindow "Bounce"
  reshapeCallback $= Just reshape

  delta     <- newIORef 0.0
  velocityX <- newIORef 0.0
  velocityY <- newIORef 0.8
  force     <- newIORef 0
  pos       <- newIORef (0.1, 0.4)
  angle     <- newIORef 0

  setProjection
  setLights
  setMaterial

  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboardMouse force velocityX velocityY pos)
  idleCallback          $= Just (idle angle delta velocityX velocityY pos)
  displayCallback       $= display velocityX velocityY angle pos

  mainLoop