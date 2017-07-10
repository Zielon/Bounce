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

instance Floorable Floor where
    setColor floor color = 
        Floor (top_left floor) (top_right floor) (bottom_left floor) (bottom_right floor) (id floor) color