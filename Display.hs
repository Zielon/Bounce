module Display (idle, display) where
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Fonts
import Control.Monad
import Data.IORef
import System.IO
import Text.Printf

import Control.Concurrent
import Points
import FloorGenerator

display :: IORef GLfloat -> 
           IORef GLfloat -> 
           IORef GLfloat -> 
           IORef (GLfloat, GLfloat) -> 
           IORef [Floor] ->
           DisplayCallback
display velocityX velocityY angle pos floor = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  fls     <- get floor
  (x',y') <- get pos
  forM_ fls $ \f -> renderPrimitive Polygon $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ getPoints f
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    scale 0.5 0.5 (0.5::GLfloat)
    renderObject Solid $ Sphere' 0.1 64 64
  swapBuffers

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)  
  postRedisplay Nothing