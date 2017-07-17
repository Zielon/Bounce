module GameObjects.Positionable(
    Positionable(..),
    Coords
) where

import Graphics.UI.GLUT

import GameObjects.Global

class (Ord a, Eq a) => Positionable a where
    getMin    :: a -> Coords
    getMax    :: a -> Coords
    getCoord  :: a -> Coords
    getX      :: a -> GLfloat
    getY      :: a -> GLfloat
    getWidth  :: a -> GLfloat
    getHeight :: a -> GLfloat