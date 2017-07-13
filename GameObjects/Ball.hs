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

instance Positionable Ball where
    getMin ball = ((x' - radius'), (y' - radius'))
                  where x'      = x ball
                        y'      = y ball
                        radius' = radius ball

    getMax ball = ((x' + radius'), (y' + radius'))
                  where x'      = x ball
                        y'      = y ball
                        radius' = radius ball

instance Bounceable Ball where
    getPosition ball     = ((x ball), (y ball))

    getVelocity ball     = ((velocityX ball), (velocityY ball))

    setPosition ball fun = Ball x' y' (velocityX ball) (velocityY ball) (radius ball) (score ball) (lastFloor ball)
                           where (x',y') = fun (x ball, y ball) 

    setVelocity ball fun = Ball (x ball) (y ball) vX vY (radius ball) (score ball) (lastFloor ball)
                           where (vX, vY) = fun (velocityX ball, velocityY ball)

    setLastFloor ball f  = Ball (x ball) (y ball) (velocityX ball) (velocityY ball) (radius ball) (score ball) f

    updateScore  ball f  = if (lastFloor ball) /= f
                           then Ball (x ball) (y ball) (velocityX ball) (velocityY ball) (radius ball) ((score ball) + 1) f
                           else ball