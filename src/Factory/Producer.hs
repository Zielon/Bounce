module Factory.Producer where

import Graphics.UI.GLUT

import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon
import Common.Drawable

import Data.Map

-- Smart constructors

polygon :: Int -> Vector -> [Vector] -> GameObject
polygon i v e = GameObject (GamePolygon i v e)

ball :: Int -> Vector -> Vector -> GLfloat -> Int -> Int -> GameObject
ball i p v r s f = GameObject (Ball i p v r s f)

getArenaObjectsMap :: Map Int GameObject
getArenaObjectsMap = fromList [ -- The polygons section
                                (6, polygon 6 (0.0, 0.0) [(0.1, 0.2), (0.1, 0.4), (0.2, 0.4)]),
                            --    (3, polygon 3 (0.0, 0.0) [(-0.8, -0.8), (-0.8, -0.7),(0.8, -0.7), (0.8, -0.8)]),
                             --   (4, polygon 4 (0.0, 0.0) [(-0.5, -0.6), (-0.4, 0.4), (-0.5, 0.4)]),
                             --   (8, polygon 8 (0.0, 0.0) [(0.5, -0.6), (0.4, 0.0), (0.5, 0.0)]),
                               (1, polygon 1 (0.0, 0.0) [(-0.2, 0.6), (0.2, 0.8), (0.1, 0.6)]),
                                -- The balls section
                                (14, ball 14 (0.4, 0.4)  (0.0, 0.0)  0.1 0 0),
                                (2,  ball 2  (0.7, 0.8)  (0.0, 0.0) 0.1 0 0),
                                (5,  ball 5  (-0.3, 0.4) (0.0, 0.0) 0.03 0 0),
                                (11, ball 11 (0.7, 0.1)  (0.0, 0.0) 0.09 0 0),
                                (12, ball 12 (-0.7, 0.8) (0.0, 0.0) 0.07 0 0),
                                (13, ball 13 (0.1, -0.1) (0.0, 0.0) 0.17 0 0)]