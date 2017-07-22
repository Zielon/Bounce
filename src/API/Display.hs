module API.Display (
  idle, 
  display)
where

import Prelude            hiding (id)
import Graphics.UI.GLUT   as G
import Graphics.UI.GLUT.Fonts
import Control.Monad
import Data.IORef
import System.IO
import Text.Printf
import Control.Concurrent
import Data.Map

import API.Points
import API.Environment

import GameObjects.Objects.Ball
import GameObjects.Objects.Floor     as F
import GameObjects.Objects.ForceBar
import GameObjects.Objects.Polygon   as P
import GameObjects.Objects.BaseClass as Base

import GameArea.FloorEngine

display :: IORef Ball    ->
           IORef GLfloat ->
           IORef (Map Int Floor) ->
           IORef (Map Int GamePolygon) ->
           IORef GLfloat ->
           DisplayCallback
display ball angle floors polygons force = do 
  clear [ColorBuffer, DepthBuffer] -- clear depth buffer, too
  clear [ColorBuffer]
  loadIdentity
  
  ball'     <- get ball
  force'    <- get force
  floors'   <- get floors
  polygons' <- get polygons

  let (x', y') = getCenter ball'

  -- | Render section ----------------------

  -- | Polygons
  forM_ polygons' $ \polygon -> draw polygon

  -- | Force Bar
  preservingMatrix $ do
      getColor3f (1, 0, 0)
      renderPrimitive G.Polygon $ mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) $ GameArea.FloorEngine.getPoints $ getBar force'
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
  -- forM_ floors' $ \f -> preservingMatrix $ do
  --     renderPrimitive G.Polygon $ mapM_ (\(x, y) -> getColor3f' (color3f f) >> (vertex $ Vertex3 x y 0)) $ getPoints f
  --     let (x,y) = top_left f
  --     getColor3f (0, 0, 0)
  --     translate $ Vector3 x y 0
  --     rasterPos (Vertex2 (0.025::GLfloat) (-0.045::GLfloat))
  --     renderString Helvetica18 $ printf "%d" (F.id f)

  -- | Ball
  draw ball'

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