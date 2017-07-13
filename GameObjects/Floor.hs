module GameObjects.Floor where

import Graphics.UI.GLUT
import Prelude hiding (floor, fst, snd, id)

import GameObjects.Positionable

-- | (x,y,z)
type Point = (GLfloat,GLfloat,GLfloat)

fst :: Point -> GLfloat
fst (x, _, _) = x

snd :: Point -> GLfloat
snd (_, y, _) = y

data Floor = Floor { 
    x            :: GLfloat,
    y            :: GLfloat,
    top_left     :: Point, 
    top_right    :: Point,
    bottom_left  :: Point,
    bottom_right :: Point,
    id           :: Int,
    color3f      :: Point,
    width        :: GLfloat,
    height       :: GLfloat
}

class Floorable a where
    setColor :: a -> Point -> a
    setY     :: GLfloat -> GLfloat -> a -> a

instance Eq Floor where
    (==) a b = id a == id b
    (/=) a b = id a /= id b

instance Ord Floor where
    compare a b = (id a) `compare` (id b)
    (<) a b     = id a < id b
    (>=) a b    = id a >= id b
    (>) a b     = id a > id b
    (<=) a b    = id a <= id b

instance Positionable Floor where
    getMin floor   = (min_x, min_y) where (min_x, min_y, _) = bottom_left floor
    getMax floor   = (max_x, max_y) where (max_x, max_y, _) = top_right floor
    getX   floor   = x floor
    getY   floor   = y floor
    getCoord floor = (x floor, y floor)
    getWidth floor = width floor
    getHeight floor = height floor

instance Floorable Floor where
    setColor floor color = Floor (x floor) (y floor) (top_left floor) (top_right floor) (bottom_left floor) (bottom_right floor) (id floor) color (width floor) (height floor)
    setY minY maxY floor = Floor (x floor) (y floor) (tl_x, maxY, tl_z) (tr_x, maxY, tr_z) (bl_x, minY, bl_z) (br_x, minY, br_z) identifier color (width floor) (height floor)
        where (tl_x, tl_y, tl_z) = top_left floor
              (bl_x, bl_y, bl_z) = bottom_left floor  
              (br_x, br_y, br_z) = bottom_right floor
              (tr_x, tr_y, tr_z) = top_right floor
              identifier         = id floor
              color              = color3f floor