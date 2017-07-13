module API.Helpers where

import Graphics.UI.GLUT

data Cond a = a :? a
 
infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

clamp :: GLfloat -> GLfloat -> GLfloat -> GLfloat
clamp v min max = Prelude.max min (Prelude.min max v)

vectorLenght :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat
vectorLenght (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2-y1)^2)