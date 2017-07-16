module Collision.Helpers where

import Graphics.UI.GLUT

type Vector = (GLfloat, GLfloat)

clamp :: GLfloat -> GLfloat -> GLfloat -> GLfloat
clamp v min max = Prelude.max min (Prelude.min max v)

lenght :: Vector -> Vector -> GLfloat
lenght (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2-y1)^2)

magnitude :: Vector -> GLfloat
magnitude (x,y) = sqrt(x^2+y^2)

normalize :: Vector -> Vector
normalize (x,y) = (x/m, y/m) where m = magnitude (x,y)

(+.) :: Vector -> Vector -> Vector
(+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(-.) :: Vector -> Vector -> Vector
(-.) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)