module FloorGenerator where

import Graphics.UI.GLUT

data Floor = Floor { 
    top_left     :: (GLfloat,GLfloat,GLfloat), 
    top_right    :: (GLfloat,GLfloat,GLfloat),
    bottom_left  :: (GLfloat,GLfloat,GLfloat),
    bottom_right :: (GLfloat,GLfloat,GLfloat)
}

getFloors :: [(GLfloat, GLfloat)] -> [Floor]
getFloors list = map (\(x,y) -> sfloor x y) list

getPoints :: Floor -> [(GLfloat,GLfloat,GLfloat)]
getPoints floor = [top_left floor, bottom_left floor, bottom_right floor, top_right floor]

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

getBottom :: [(GLfloat,GLfloat,GLfloat)]
getBottom = [(-1.0,-0.95,0.0),(1.0,-0.95,0.0)]
