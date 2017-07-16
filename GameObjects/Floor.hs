module GameObjects.Floor where

import Graphics.UI.GLUT
import Prelude hiding (floor, id)

import GameObjects.Positionable
import GameObjects.General

data Floor = Floor { 
    x            :: GLfloat,
    y            :: GLfloat,
    top_left     :: Point, 
    top_right    :: Point,
    bottom_left  :: Point,
    bottom_right :: Point,
    id           :: Int,
    color3f      :: (GLfloat, GLfloat, GLfloat),
    width        :: GLfloat,
    height       :: GLfloat
}

class Floorable a where
    setColor :: a -> (GLfloat, GLfloat, GLfloat) -> a
    setY     :: GLfloat -> GLfloat -> a -> a

instance Eq Floor where
    (==) a b = id a == id b
    (/=) a b = id a /= id b

instance Ord Floor where
    compare a b = (id a) `compare` (id b)
    (<)  a b    = id a <  id b
    (>=) a b    = id a >= id b
    (>)  a b    = id a >  id b
    (<=) a b    = id a <= id b

instance Positionable Floor where
    getMin floor    = (min_x, min_y) where (min_x, min_y) = bottom_left floor
    getMax floor    = (max_x, max_y) where (max_x, max_y) = top_right floor
    getX   floor    = x floor
    getY   floor    = y floor
    getCoord floor  = (x floor, y floor)
    getWidth floor  = width floor
    getHeight floor = height floor

instance Floorable Floor where
    setColor floor color = Floor (x floor) (y floor) (top_left floor) (top_right floor) (bottom_left floor) (bottom_right floor) (id floor) color (width floor) (height floor)
    setY minY maxY floor = Floor (x floor) (y floor) (tl_x, maxY) (tr_x, maxY) (bl_x, minY) (br_x, minY) identifier color (width floor) (height floor)
        where (tl_x, tl_y) = top_left floor
              (bl_x, bl_y) = bottom_left floor  
              (br_x, br_y) = bottom_right floor
              (tr_x, tr_y) = top_right floor
              identifier         = id floor
              color              = color3f floor