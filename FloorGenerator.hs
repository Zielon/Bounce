module FloorGenerator where

import Graphics.UI.GLUT
import Prelude hiding (snd)
import System.Random

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

evaluate :: Floor -> (Point -> Point) -> Floor
evaluate flr fun = do
    if snd tl > -0.95 then Floor (fun tl) (fun tr) (fun bl) (fun br)
    else sfloor x 1.3
    where tl = top_left flr
          bl = bottom_left flr
          br = bottom_right flr
          tr = top_right flr
          x  = Prelude.fst $ random (mkStdGen 42) :: GLfloat

changeY :: Floor -> GLfloat -> Floor
changeY flr indicator = evaluate flr $ \(x, y, z) -> (x, y - indicator, z)

moveDown :: [Floor] -> GLfloat -> [Floor]
moveDown floors value = map (\f -> changeY f value) floors

-- Floor [game element]
-- r - width 
-- t - thickness
gfloor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Floor
gfloor x y r t = Floor tl tr bl br
    where tl = ( x - r, y + t, 0.0)  -- | top left
          bl = ( x - r, y - t, 0.0)  -- | bottom left
          br = ( x + r, y - t, 0.0)  -- | bottom right
          tr = ( x + r, y + t, 0.0)  -- | top right 

sfloor x y = gfloor x y 0.3 0.3

getBottom :: [Point]
getBottom = [(-1.0,-0.95,0.0),(1.0,-0.95,0.0)]
