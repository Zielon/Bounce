module API.Helpers where

import Graphics.UI.GLUT

type Vector = (GLfloat, GLfloat)

data Cond a = a :? a
 
infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y