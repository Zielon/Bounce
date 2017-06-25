module Display (idle, display) where
 
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Fonts
import Control.Monad
import Data.IORef
import System.IO
import Text.Printf

import Control.Concurrent
import Points
import Gravity

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display velocityX velocityY angle pos = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  renderPrimitive Lines $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z)
     [(-1::GLfloat,-0.95::GLfloat,0::GLfloat), (1::GLfloat,-0.95::GLfloat,0::GLfloat)]
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    rotate a $ Vector3 0 0 1
    rotate a $ Vector3 0 0.1 1 -- changed y-component a bit to show off cube corners
    scale 0.5 0.5 (0.5::GLfloat)
    renderObject Solid $ Sphere' 0.1 64 64
  swapBuffers

idle :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> IdleCallback
idle angle delta velocityX velocityY pos = do
  d      <- get delta
  vX     <- get velocityX
  vY     <- get velocityY
  (x, y) <- get pos
  putStrLn $ printf "x -> %f v: %f | y -> %f v: %f" x vX y vY
  angle $~! (+ d)
  updateGravity velocityX velocityY pos >> postRedisplay Nothing
  if vY < 0.005 && y < -0.90 then pos $~! \(x',y') -> (x', -0.90)
  else return ()
