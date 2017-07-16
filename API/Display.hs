module API.Display (
  idle, 
  display)
where

import Prelude hiding (id)
import Graphics.UI.GLUT as G
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
import GameObjects.Floor   as F
import GameObjects.ForceBar
import GameObjects.Polygon as P

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

  let polygon = P.Polygon 1 [(0.1, 0.2), (0.2, 0.4), (0.1, 0.4), (0.4, 0.4)]
  renderPrimitive G.Polygon $ mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) $ P.points polygon

  -- | Render section ----------------------

  -- | Force Bar
  preservingMatrix $ do
      getColor3f (1, 0, 0)
      renderPrimitive G.Polygon $ mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) $ getPoints $ getBar force'
      translate $ Vector3 (-0.95::GLfloat) (0.95::GLfloat) 0
      rasterPos (Vertex2 (0.0::GLfloat) (-0.025::GLfloat))
      renderString Helvetica18 $ printf "%.1f%%" (force' * 10)

  -- | Points
  preservingMatrix $ do
      getColor3f (1, 0, 1)
      translate $ Vector3 (0.65::GLfloat) (0.95::GLfloat) 0
      rasterPos (Vertex2 (0.0::GLfloat) (-0.025::GLfloat))
      renderString Helvetica18 $ printf "Points %d" (score ball')

  -- | Floors
  forM_ floors' $ \f -> preservingMatrix $ do
      renderPrimitive G.Polygon $ mapM_ (\(x, y) -> getColor3f' (color3f f) >> (vertex $ Vertex3 x y 0)) $ getPoints f
      let (x,y) = top_left f
      getColor3f (0, 0, 0)
      translate $ Vector3 x y 0
      rasterPos (Vertex2 (0.025::GLfloat) (-0.045::GLfloat))
      renderString Helvetica18 $ printf "%d" (F.id f)

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

    getColor3f' :: (GLfloat, GLfloat, GLfloat) -> IO ()
    getColor3f' (r, g, b) = color $ Color3 r g (b :: GLfloat)

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)  
  postRedisplay Nothing