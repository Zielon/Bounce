module API.Display (
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
import Data.Map

import API.Points
import API.Environment
import GameObjects.Ball
import GameObjects.Floor
import GameObjects.ForceBar
import Engines.FloorEngine

display :: IORef Ball    ->
           IORef GLfloat ->      
           IORef (Map Int Floor) ->
           IORef GLfloat ->
           DisplayCallback
display ball angle floors force = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  
  ball'   <- get ball
  force'  <- get force
  floors' <- get floors

  let (x', y') = getPosition ball'

  -- | Render section ----------------------

  -- | Force Bar
  preservingMatrix $ do
      getColor3f (1, 0, 0)
      renderPrimitive Polygon $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) $ getPoints $ getBar force'
      translate $ Vector3 (-0.95::GLfloat) (0.95::GLfloat) 0
      rasterPos (Vertex2 (0.0::GLfloat) (-0.025::GLfloat))
      renderString Helvetica18 $ printf "%.1f%%" (force' * 10)

  -- | Points
  preservingMatrix $ do
      getColor3f (1, 0, 1)
      translate $ Vector3 (0.65::GLfloat) (0.95::GLfloat) 0
      rasterPos (Vertex2 (0.0::GLfloat) (-0.025::GLfloat))
      renderString Helvetica18 $ printf "Points %d" (getScore ball')

  -- | Floors
  forM_ floors' $ \f -> renderPrimitive Polygon $ do 
    mapM_ (\(x, y, z) -> getColor3f' (color3f f) >> (vertex $ Vertex3 x y z)) $ getPoints f

  -- | Ball
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    a <- get angle
    scale 0.5 0.5 (0.5::GLfloat)
    getColor3f (1, 0, 0)
    renderObject Solid $ Sphere' 0.1 64 64
  
  swapBuffers
  where 
    getColor3f :: (Float, Float, Float) -> IO ()
    getColor3f (r, g, b) = color $ Color3 r g (b :: GLfloat)

    getColor3f' :: Point -> IO ()
    getColor3f' (r, g, b) = color $ Color3 r g (b :: GLfloat)

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)  
  postRedisplay Nothing