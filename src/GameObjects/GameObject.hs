{-# LANGUAGE ExistentialQuantification #-}

module GameObjects.GameObject(
    Vector,
    GameObject_(..),
    GameObject(..),
    ObjectType(..),
    getColor3f
) where

import Graphics.UI.GLUT
import Data.List

type Vector = (GLfloat, GLfloat)

data ObjectType = PolygonType | BallType deriving (Eq, Ord)

-- | The general class instantiated by every game's object
-- |
class GameObject_ a where

    -- SETTERS
    setOffset   :: Vector -> a -> a
    setVelocity :: Vector -> a -> a

    -- GETTERS
    getVelocity :: a -> Vector
    getCenter   :: a -> Vector
    getPoints   :: a -> [Vector]
    getId       :: a -> Int
    getEdges    :: a -> [Vector]
    getType     :: a -> ObjectType

    -- OTHERS
    projection  :: Vector -> a -> (GLfloat, GLfloat)
    draw        :: a -> IO ()

data GameObject = forall a. GameObject_ a => GameObject a

getColor3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
getColor3f x y z = color $ Color3 ((x+1)/2) ((y+1)/2) (((z+1)/2) :: GLfloat)