module Collision.Operations where

import Data.List

import API.Ternary
import Graphics.UI.GLUT
import GameObjects.Global

clamp :: GLfloat -> GLfloat -> GLfloat -> GLfloat
clamp v min max = Prelude.max min (Prelude.min max v)

-- | Length between two vectors
--
lenght :: Vector -> Vector -> GLfloat
lenght (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2-y1)^2)

magnitude :: Vector -> GLfloat
magnitude (x, y) = sqrt(x*x + y*y)

normalize :: Vector -> Vector
normalize (x, y) = (x/m, y/m) where m = magnitude (x,y)

-- | Get a perpendicular vector
-- For a given vector the perpendicular vectors are (-y,x), (y,-x)
-- where the dot product is 0; example (2,7) and (-7,2)
--
perpendicular :: Vector -> Vector
perpendicular (x, y) = (-y, x)

-- | Scalar product of two vectors
--
dotProduct :: Vector -> Vector -> GLfloat
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

-- | Calculate the distance between [minA, maxA] and [minB, maxB]
--   The distance will be negative if the intervals overlap
--
calculateIntervalDistance :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat
calculateIntervalDistance (minA, maxA) (minB, maxB) = minA < minB ? (minB - maxA) :? (minA - maxB)

-- OPERATORS

(--.) :: Vector -> Vector
(--.) (x, y) = (-x, -y)

(+.) :: Vector -> Vector -> Vector
(+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(-.) :: Vector -> Vector -> Vector
(-.) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

(*.) :: Vector -> GLfloat -> Vector
(*.) (x1, y1) a = (x1 * a, y1 * a)
