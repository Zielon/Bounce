import Graphics.UI.GLUT
import Data.IORef
import Bindings
import Environment
import Keys

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  initialWindowSize $= Size 500 500
  createWindow "Bouncing"
  reshapeCallback $= Just reshape

  delta     <- newIORef 0.0
  velocityX <- newIORef 0.0
  velocityY <- newIORef 0.8
  force     <- newIORef 0.0
  pos       <- newIORef (0.0, 0.8)
  angle     <- newIORef 0
  keys      <- getKeys

  setProjection
  setLights
  setMaterial

  clearColor            $= Color4 255.0 255.0 255.0 255.0
  keyboardMouseCallback $= Just (keyboardMouse force velocityX velocityY keys delta pos)
  idleCallback          $= Just (idle angle delta velocityX velocityY pos)
  displayCallback       $= display velocityX velocityY angle pos
  
  mainLoop