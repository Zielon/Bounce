module GameObjects.Positionable(
    Positionable(..),
    Vector
) where

import Graphics.UI.GLUT

import GameObjects.Global

class (Ord a, Eq a) => Positionable a where
    getMin    :: a -> Vector
    getMax    :: a -> Vector
    getCoord  :: a -> Vector
    getX      :: a -> GLfloat
    getY      :: a -> GLfloat
    getWidth  :: a -> GLfloat
    getHeight :: a -> GLfloat