module GameObjects.Ball(
    Ball(..),
    Bounceable(..)
) where

import Graphics.UI.GLUT

data Ball = Ball {
    getX         :: GLfloat,
    getY         :: GLfloat,
    getVelocityX :: GLfloat,
    getVelocityY :: GLfloat,
    getScore     :: Int,
    getLastFloor :: Int
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
    getPosition ball     = ((getX ball), (getY ball))

    getVelocity ball     = ((getVelocityX ball), (getVelocityY ball))

    setPosition ball fun = Ball x y (getVelocityX ball) (getVelocityY ball) (getScore ball) (getLastFloor ball)
                           where (x,y) = fun (getX ball, getY ball) 

    setVelocity ball fun = Ball (getX ball) (getY ball) vX vY (getScore ball) (getLastFloor ball)
                           where (vX, vY) = fun (getVelocityX ball, getVelocityY ball)

    setLastFloor ball f  = Ball (getX ball) (getY ball) (getVelocityX ball) (getVelocityY ball) (getScore ball) f

    updateScore  ball f  = if (getLastFloor ball) /= f
                           then Ball (getX ball) (getY ball) (getVelocityX ball) (getVelocityY ball) ((getScore ball) + 1) f
                           else ball