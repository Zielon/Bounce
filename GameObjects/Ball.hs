module GameObjects.Ball(
    Ball(..),
    Bounceable(..)
) where

import Graphics.UI.GLUT

import GameObjects.Positionable

data Ball = Ball {
    x         :: GLfloat,
    y         :: GLfloat,
    velocityX :: GLfloat,
    velocityY :: GLfloat,
    radius    :: GLfloat,
    score     :: Int,
    lastFloor :: Int
}

-- | Mainly setters and getters
class Bounceable a where
    getPosition  :: a -> (GLfloat, GLfloat)
    getVelocity  :: a -> (GLfloat, GLfloat)
    setPosition  :: a -> ((GLfloat, GLfloat) -> (GLfloat, GLfloat)) -> a
    setVelocity  :: a -> ((GLfloat, GLfloat) -> (GLfloat, GLfloat)) -> a
    setLastFloor :: a -> Int -> a
    updateScore  :: a -> Int -> a

instance Bounceable Ball where
    getPosition ball     = ((x ball), (y ball))
    getVelocity ball     = ((velocityX ball), (velocityY ball))
    setPosition ball fun = ball { x = _x, y = _y } where (_x, _y) = fun (x ball, y ball)
    setVelocity ball fun = ball { velocityX = vX, velocityY = vY } where (vX, vY) = fun (velocityX ball, velocityY ball)
    setLastFloor ball f  = ball { lastFloor = f }
    updateScore  ball f  = if (lastFloor ball) /= f then ball { score = s + 1 } else ball
                           where s = score ball