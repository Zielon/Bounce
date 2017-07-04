module FloorGenerator where

import Graphics.UI.GLUT
import Prelude hiding (snd)

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
    bottom_right :: Point
}

getFloors :: [(GLfloat, GLfloat)] -> [Floor]
getFloors list = map (\(x,y) -> sfloor x y) list

getPoints :: Floor -> [Point]
getPoints floor = [top_left floor, bottom_left floor, bottom_right floor, top_right floor]

evaluate :: GLfloat -> Floor -> (Point -> Point) -> Floor
evaluate x flr fun =
    if snd tl > -1.0 then Floor (fun tl) (fun tr) (fun bl) (fun br)
    else sfloor x 1.0
    where tl = top_left flr
          bl = bottom_left flr
          br = bottom_right flr
          tr = top_right flr

moveDown :: GLfloat -> [Floor] -> GLfloat -> [Floor]
moveDown value floors indicator = map (\f -> evaluate value f $ \(x, y, z) -> (x, y - indicator, z)) floors

-- Floor [game element]
-- r - width 
-- t - thickness
gfloor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Floor
gfloor x y r t = Floor tl tr bl br
    where tl = ( x - r, y + t, 0.0)  -- | top left
          bl = ( x - r, y - t, 0.0)  -- | bottom left
          br = ( x + r, y - t, 0.0)  -- | bottom right
          tr = ( x + r, y + t, 0.0)  -- | top right 

sfloor x y = gfloor x y 0.3 0.025

getMockedFloors :: [(GLfloat, GLfloat)]
getMockedFloors = [(0.5, 0.5), (-0.5, -0.5), (0.7, -0.8), (0.7, 0.8), (-0.7, 0.5), (-0.1, 0.8)]