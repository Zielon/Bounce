module Display (
  idle, 
  display)
where
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Fonts
import Control.Monad
import Data.IORef
import System.IO
import Text.Printf
import Control.Concurrent

import Points
import FloorEngine
import ForceBar
import Environment

display :: IORef GLfloat -> 
           IORef GLfloat -> 
           IORef GLfloat -> 
           IORef (GLfloat, GLfloat) -> 
           IORef [Floor] ->
           IORef GLfloat ->
           DisplayCallback
display velocityX velocityY angle pos floor force = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  force'  <- get force
  fls     <- get floor
  (x',y') <- get pos
  
  renderPrimitive Polygon $ do 
    color3f 1 0 0
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ getPoints $ getBar force'
  
  forM_ fls $ \f -> renderPrimitive Polygon $ do 
    mapM_ (\(x, y, z) -> (color3f ((x+1)/2) ((y+1)/2) ((z+1)/2)) >> (vertex $ Vertex3 x y z)) $ getPoints f
  
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    scale 0.5 0.5 (0.5::GLfloat)
    color3f 1 0 0
    renderObject Solid $ Sphere' 0.1 64 64
  swapBuffers
  
  where color3f r g b = color $ Color3 r g (b :: GLfloat)

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)  
  postRedisplay Nothing