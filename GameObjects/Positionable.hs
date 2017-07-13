module GameObjects.Positionable where

import Graphics.UI.GLUT

type Coords = (GLfloat, GLfloat)

class Positionable a where
    getMin :: a -> Coords
    getMax :: a -> Coords