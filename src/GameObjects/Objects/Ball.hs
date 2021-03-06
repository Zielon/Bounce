{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module GameObjects.Objects.Ball(
    Ball(..),
    Bounceable(..),
    GameObject(..),
    Type(..),
    Vector
) where

import Graphics.UI.GLUT
import Prelude hiding (id)
import Text.Printf
import GHC.Float
import Data.Aeson (ToJSON)
import GHC.Generics

import API.Ternary
import GameObjects.GameObject
import Common.Drawable
import Collision.VectorOperations

data Ball = Ball {
    id        :: Int,
    center    :: Vector,
    velocity  :: Vector,
    radius    :: GLfloat,
    score     :: Int,
    lastFloor :: Int,
    hovered   :: Bool
} deriving (Show, Generic, ToJSON)

-- | Class characteristic only for the Ball object
--
class Bounceable a where
    setLastFloor :: a -> Int -> a
    updateScore  :: a -> Int -> a

instance GameObject_ Ball where 
    setOffset (x1, y1) ball = ball { center = (x1 + x2, y1 + y2), velocity = (0.0, 0.0) } where (x2, y2) = center ball
    setVelocity vector ball = ball { velocity = vector }
    setHovered bool ball = ball { hovered = bool }
    getVelocity ball = velocity ball
    getHovered ball = hovered ball
    getCenter ball = center ball
    getId ball = id ball
    getType ball = BallType
    getRadius ball = radius ball 
    getPoints ball         = error "Not implemented exception"
    getEdges ball          = error "Not implemented exception"
    projection vector axis = error "Not implemented exception"
    draw ball = do
        let (x,y) = center ball
            diameter = (radius ball * 2.0)
        preservingMatrix $ do
            translate $ Vector3 x y 0
            scale 0.5 0.5 (0.5::GLfloat)
            hovered ball == False ? getColor3f (x) (y/2) 0 :? getColor3f 1 1 0
            renderObject Solid $ Sphere' (float2Double diameter) 64 64
            getColor3f (-1) (-1) (-1)
            rasterPos (Vertex2 (-0.03::GLfloat) (-0.03::GLfloat))
            renderString Helvetica18 $ printf "%d" (id ball)

instance Bounceable Ball where
    setLastFloor ball f  = ball { lastFloor = f }
    updateScore  ball f  = if (lastFloor ball) /= f then ball { score = s + 1 } else ball
                           where s = score ball

instance Drawable_ Ball where
    render ball = draw ball

instance Eq Ball where
    (==) a b = id a == id b
    (/=) a b = id a /= id b

instance Ord Ball where
    compare a b = (id a) `compare` (id b)
    (<)  a b    = id a <  id b
    (>=) a b    = id a >= id b
    (>)  a b    = id a >  id b
    (<=) a b    = id a <= id b