module API.Helpers where

import Graphics.UI.GLUT

type Vector = (GLfloat, GLfloat)

data Cond a = a :? a
 
infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

clamp :: GLfloat -> GLfloat -> GLfloat -> GLfloat
clamp v min max = Prelude.max min (Prelude.min max v)

vecLenght :: Vector -> Vector -> GLfloat
vecLenght (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2-y1)^2)

vecAdd :: Vector -> Vector -> Vector
vecAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 
