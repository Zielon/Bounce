module Display (idle, display) where
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Fonts
import Control.Monad
import Data.IORef
import System.IO
import Text.Printf

import Control.Concurrent
import Points
import Physics
import FloorGenerator

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> [Floor] -> DisplayCallback
display velocityX velocityY angle pos floors = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  forM_ floors $ \f -> renderPrimitive Polygon $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ getPoints f
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 0 1
    rotate a $ Vector3 0 0.1 1 -- changed y-component a bit to show off cube corners
    scale 0.5 0.5 (0.5::GLfloat)
    renderObject Solid $ Sphere' 0.1 64 64
  swapBuffers

idle :: IORef GLfloat -> 
        IORef GLfloat -> 
        IORef GLfloat -> 
        IORef GLfloat -> 
        IORef (GLfloat, GLfloat) -> 
        [Floor] -> 
        IdleCallback
idle angle delta velocityX velocityY pos floors = do
  d      <- get delta
  vX     <- get velocityX 
  vY     <- get velocityY
  (x, y) <- get pos
  putStrLn $ printf "Idle:: x -> %.8f v: %.8f | y -> %.8f v: %.8f" x vX y vY

  angle $~! (+ d)
  updateGravity velocityX velocityY floors pos

  if vY < 0.003 && y < -0.90 then pos $~! (\(x',y') -> (x', -0.90)) >> postRedisplay Nothing
  else postRedisplay Nothing