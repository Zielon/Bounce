module GameObjects.Objects.BaseClass(
    Vector,
    BaseClass(..)
) where

import Graphics.UI.GLUT

type Vector = (GLfloat, GLfloat)

-- | The general class instantiated by every game's object
-- |
class BaseClass a where
    setOffset   :: Vector -> a -> a
    setVelocity :: Vector -> a -> a
    getCenter   :: a -> Vector
    draw        :: a -> IO ()