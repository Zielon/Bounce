module GameObjects.Objects.BaseClass(
    Vector,
    BaseClass(..),
    getColor3f
) where

import Graphics.UI.GLUT
import Data.List

type Vector = (GLfloat, GLfloat)

-- | The general class instantiated by every game's object
-- |
class BaseClass a where
    -- SETTERS
    setOffset   :: Vector -> a -> a
    setVelocity :: Vector -> a -> a
    -- GETTERS
    getVelocity :: a -> Vector
    getCenter   :: a -> Vector
    getPoints   :: a -> [Vector]
    -- OTHERS
    projection  :: Vector -> a -> (GLfloat, GLfloat)
    draw        :: a -> IO ()

getColor3f x y z = color $ Color3 ((x+1)/2) ((y+1)/2) (((z+1)/2) :: GLfloat)