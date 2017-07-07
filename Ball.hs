module Ball(
    Ball(..),
    Bounceable(..)
) where

import Graphics.UI.GLUT

data Ball = Ball {
    getX         :: GLfloat,
    getY         :: GLfloat,
    getVelocityX :: GLfloat,
    getVelocityY :: GLfloat,
    getScore     :: Int
}

class Bounceable a where
    getPosition :: a -> (GLfloat, GLfloat)
    getVelocity :: a -> (GLfloat, GLfloat)
    setPosition :: a -> ((GLfloat, GLfloat) -> (GLfloat, GLfloat)) -> a
    setVelocity :: a -> ((GLfloat, GLfloat) -> (GLfloat, GLfloat)) -> a

instance Bounceable Ball where
    getPosition ball     = ((getX ball), (getY ball))
    getVelocity ball     = ((getVelocityX ball), (getVelocityY ball))
    setPosition ball fun = Ball x y (getVelocityX ball) (getVelocityY ball) (getScore ball)
                           where (x,y) = fun (getX ball, getY ball) 
    setVelocity ball fun = Ball (getX ball) (getY ball) vX vY (getScore ball)
                           where (vX, vY) = fun (getVelocityX ball, getVelocityY ball)