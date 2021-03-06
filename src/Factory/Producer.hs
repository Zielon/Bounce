module Factory.Producer where

import Graphics.UI.GLUT

import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon
import GameObjects.Objects.Segment
import Common.Drawable

import Collision.VectorOperations    as O
import Data.Map                      as M

-- Smart constructors

polygon :: Int -> Vector -> [Vector] -> GameObject
polygon i v e = GameObject (GamePolygon i v e False)

ball :: Int -> Vector -> Vector -> GLfloat -> Int -> Int -> GameObject
ball i p v r s f = GameObject (Ball i p v r s f False)

getArenaObjectsMap :: Map Int GameObject
getArenaObjectsMap = M.fromList [ -- The polygons section
                                (6, polygon 6 (0.0, 0.0) [(0.1, 0.2), (0.1, 0.4), (0.2, 0.4)]),
                                (3, polygon 3 (0.0, 0.0) [(0.4, -0.95), (0.2, -0.8), (0.2, -0.65), (0.4, -0.5), (0.6, -0.65), (0.6, -0.8)]),
                                (4, polygon 4 (0.0, 0.0) [(-0.5, -0.6), (-0.4, 0.4), (-0.5, 0.4)]),
                                (8, polygon 8 (0.0, 0.0) [(0.5, -0.5), (0.4, 0.1), (0.5, 0.1)]),
                                (1, polygon 1 (0.0, 0.0) [(-0.2, 0.6), (0.2, 0.8), (0.1, 0.6)]),
                                -- The balls section
                                (14, ball 14 (0.4, 0.4)  (0.0, 0.0)  0.1 0 0),
                                (2,  ball 2  (0.5, 0.8)  (0.0, 0.0) 0.1 0 0),
                                (5,  ball 5  (-0.3, 0.4) (0.0, 0.0) 0.03 0 0),
                                (11, ball 11 (0.7, 0.1)  (0.0, 0.0) 0.09 0 0),
                                (12, ball 12 (-0.4, 0.8) (0.0, 0.0) 0.07 0 0),
                                (13, ball 13 (0.1, -0.1) (0.0, 0.0) 0.17 0 0)]

getSegments :: GLfloat -> GLfloat -> Vector -> [Segment]
getSegments i l mouse = [ Segment (r,g,b) mouse $ (sin (2*pi*k/i), cos (2*pi*k/i)) *. l | k <- [1..i] ]
    where r = -0.05
          g = -0.05
          b = -0.05
