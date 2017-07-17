module GameObjects.Floor(
    Floor(..),
    Floorable(..),
    Point
) where

import Graphics.UI.GLUT
import Prelude hiding (floor, id)

import GameObjects.Global
import GameObjects.Positionable

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
    setColor floor color = floor { color3f = color }
    setY minY maxY floor = floor { top_left = (tl_x, maxY), 
                                   top_right = (tr_x, maxY), 
                                   bottom_left = (bl_x, minY), 
                                   bottom_right = (br_x, minY)}

        where (tl_x, _) = top_left floor
              (bl_x, _) = bottom_left floor  
              (br_x, _) = bottom_right floor
              (tr_x, _) = top_right floor