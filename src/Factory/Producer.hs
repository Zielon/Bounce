module Factory.Producer where

import Graphics.UI.GLUT

import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon
import Common.Drawable

import Data.Map

polygon :: Int -> Vector -> [Vector] -> GameObject
polygon i v e = GameObject (GamePolygon i v e)

ball :: Int -> Vector -> Vector -> GLfloat -> Int -> Int -> GameObject
ball i p v r s f = GameObject (Ball i p v r s f)

getArenaObjectsMap :: Map Int GameObject
getArenaObjectsMap = fromList [ (7, polygon 7 (0.0, 0.0) [(0.1, 0.2), (0.1, 0.4), (0.2, 0.4)]),
                                (5, polygon 5 (0.0, 0.0) [(-0.8, -0.8), (-0.8, -0.7),(0.8, -0.7), (0.8, -0.8)]),
                                (4, polygon 4 (0.0, 0.0) [(-0.5, -0.6), (-0.4, 0.4), (-0.5, 0.4)]),
                                (8, polygon 8 (0.0, 0.0) [(0.5, -0.6), (0.4, 0.0), (0.5, 0.0)]),
                                (6, polygon 6 (0.0, 0.0) [(-0.2, 0.6), (0.2, 0.8), (0.1, 0.6)]),
                                -- Balls section
                                (1, ball 1 (0.4, 0.4) (0.0, 0.0)  0.1 0 0),
                                (2, ball 2 (0.7, 0.8) (0.0, 0.0) 0.12 0 0),
                                (3, ball 3 (-0.3, 0.4) (0.0, 0.0) 0.03 0 0)]