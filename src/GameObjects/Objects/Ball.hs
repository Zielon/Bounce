module GameObjects.Objects.Ball(
    Ball(..),
    Bounceable(..),
    Vector
) where

import Graphics.UI.GLUT

import GameObjects.Objects.BaseClass

data Ball = Ball {
    center    :: Vector,
    velocity  :: Vector,
    radius    :: GLfloat,
    score     :: Int,
    lastFloor :: Int
}

class Bounceable a where
    setLastFloor :: a -> Int -> a
    updateScore  :: a -> Int -> a

instance BaseClass Ball where 
    setOffset vector ball = ball { center = vector }
    setVelocity vector ball = ball { velocity = vector }
    getVelocity ball = velocity ball
    getCenter ball = center ball
    getPoints ball = [center ball]
    projection vector axis = (0.0, 0.0) -- TODO 
    draw ball = do
        let (x,y) = center ball
        preservingMatrix $ do
            translate $ Vector3 x y 0
            scale 0.5 0.5 (0.5::GLfloat)
            getColor3f 1 0 0
            renderObject Solid $ Sphere' 0.1 64 64

instance Bounceable Ball where
    setLastFloor ball f  = ball { lastFloor = f }
    updateScore  ball f  = if (lastFloor ball) /= f then ball { score = s + 1 } else ball
                           where s = score ball