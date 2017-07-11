module GameObjects.Floor where

import Graphics.UI.GLUT
import Prelude hiding (floor, fst, snd, id)

-- | (x,y,z)
type Point = (GLfloat,GLfloat,GLfloat)

fst :: Point -> GLfloat
fst (x, _, _) = x

snd :: Point -> GLfloat
snd (_, y, _) = y

data Floor = Floor { 
    top_left     :: Point, 
    top_right    :: Point,
    bottom_left  :: Point,
    bottom_right :: Point,
    id           :: Int,
    color3f      :: Point
}

class Floorable a where
    setColor :: a -> Point -> a
    setY     :: GLfloat -> GLfloat -> a -> a

instance Floorable Floor where
    setColor floor color = Floor (top_left floor) (top_right floor) (bottom_left floor) (bottom_right floor) (id floor) color  
    setY minY maxY floor = Floor (tl_x, maxY, tl_z) (tr_x, maxY, tr_z) (bl_x, minY, bl_z) (br_x, minY, br_z) identifier color
        where (tl_x, tl_y, tl_z) = top_left floor
              (bl_x, bl_y, bl_z) = bottom_left floor  
              (br_x, br_y, br_z) = bottom_right floor
              (tr_x, tr_y, tr_z) = top_right floor
              identifier         = id floor
              color              = color3f floor