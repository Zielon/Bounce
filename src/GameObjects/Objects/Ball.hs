module GameObjects.Objects.Ball(
    Ball(..),
    Bounceable(..),
    GameObject(..),
    Vector
) where

import Graphics.UI.GLUT
import Prelude hiding (id)
import Text.Printf

import GameObjects.GameObject

data Ball = Ball {
    id        :: Int,
    center    :: Vector,
    velocity  :: Vector,
    radius    :: GLfloat,
    score     :: Int,
    lastFloor :: Int
}

-- | Class characteristic only for the Ball object
--
class Bounceable a where
    setLastFloor :: a -> Int -> a
    updateScore  :: a -> Int -> a

instance GameObject Ball where 
    setOffset vector ball = ball { center = vector }
    setVelocity vector ball = ball { velocity = vector }
    getVelocity ball = velocity ball
    getCenter ball = center ball
    getPoints ball = [center ball]
    getId ball = id ball
    getEdges ball = []      -- TODO
    projection vector axis = (0.0, 0.0) -- TODO 
    draw ball = do
        let (x,y) = center ball
        preservingMatrix $ do
            translate $ Vector3 x y 0
            scale 0.5 0.5 (0.5::GLfloat)
            getColor3f 1 0 0
            renderObject Solid $ Sphere' 0.1 64 64
            getColor3f (-1) (-1) (-1)
            rasterPos (Vertex2 (-0.03::GLfloat) (-0.03::GLfloat))
            renderString Helvetica18 $ printf "%d" (id ball)

instance Bounceable Ball where
    setLastFloor ball f  = ball { lastFloor = f }
    updateScore  ball f  = if (lastFloor ball) /= f then ball { score = s + 1 } else ball
                           where s = score ball

instance Eq Ball where
    (==) a b = id a == id b
    (/=) a b = id a /= id b

instance Ord Ball where
    compare a b = (id a) `compare` (id b)
    (<)  a b    = id a <  id b
    (>=) a b    = id a >= id b
    (>)  a b    = id a >  id b
    (<=) a b    = id a <= id b
