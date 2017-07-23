module Factory.Producer where

import GameObjects.Objects.Ball     as B
import GameObjects.Objects.Polygon  as P

createBallObjects :: [(Int, Ball)]
createBallObjects = [(1, B.Ball 1 (0.9, 0.9) (0.0, 0.1) 0.05 0 0),
                     (2, B.Ball 2 (0.8, 0.8) (0.0, 0.2) 0.05 0 0), 
                     (3, B.Ball 3 (0.3, 0.7) (0.0, 0.3) 0.05 0 0)]

createPolygonObjects ::  [(Int, GamePolygon)]
createPolygonObjects = [(1, P.GamePolygon 1 (0,0) [(0.1, 0.2), (0.1, 0.4), (0.2, 0.4)]),
                        (5, P.GamePolygon 5 (0,0) [(-0.8, -0.8), (-0.8, -0.7),(0.8, -0.7), (0.8, -0.8)]),
                        (4, P.GamePolygon 4 (0,0) [(-0.5, -0.6), (-0.4, 0.4), (-0.5, 0.4)]),
                        (3, P.GamePolygon 3 (0,0) [(0.5, -0.6), (0.4, 0.0), (0.5, 0.0)]),
                        (2, P.GamePolygon 2 (0,0) [(-0.2, 0.6), (0.2, 0.8), (0.1, 0.6)])]