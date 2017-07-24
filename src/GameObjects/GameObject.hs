module GameObjects.GameObject(
    Vector,
    GameObject(..),
    GameEnum,
    getColor3f
) where

import Graphics.UI.GLUT
import Data.List
import GameObjects.GameEnum

type Vector = (GLfloat, GLfloat)

-- | The general class instantiated by every game's object
-- |
class GameObject a where

    -- SETTERS
    setOffset   :: Vector -> a -> a
    setVelocity :: Vector -> a -> a

    -- GETTERS
    getVelocity :: a -> Vector
    getCenter   :: a -> Vector
    getPoints   :: a -> [Vector]
    getId       :: a -> Int
    getEdges    :: a -> [Vector]

    -- OTHERS
    projection  :: Vector -> a -> (GLfloat, GLfloat)
    draw        :: a -> IO ()

getColor3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
getColor3f x y z = color $ Color3 ((x+1)/2) ((y+1)/2) (((z+1)/2) :: GLfloat)