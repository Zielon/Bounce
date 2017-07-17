module GameObjects.Polygon(
    GamePolygon(..),
    Polygonable(..),
    Vector,
    projection
) where

import Prelude hiding (id)
import Graphics.UI.GLUT

import GameObjects.Global
import Collision.Operations

class Polygonable a where
    getEdges :: a -> [Vector]

data GamePolygon = GamePolygon {
    id     :: Int,
    points :: [Vector]
}

instance Polygonable GamePolygon where
    getEdges polygon = map (\(a, b) -> (-.) b a ) $ (edgefiy p) ++ [(last p, head p)]
        where p = points polygon

instance Eq GamePolygon where
    (==) a b = id a == id b
    (/=) a b = id a /= id b

instance Ord GamePolygon where
    compare a b = (id a) `compare` (id b)
    (<)  a b    = id a <  id b
    (>=) a b    = id a >= id b
    (>)  a b    = id a >  id b
    (<=) a b    = id a <= id b

-- | Projection of each point on the axis to find the length on the perpendicular axis
--  @axis    - perpendicular vector to a selected axis
--  @polygon - the given polygon
--
projection :: Vector -> GamePolygon -> (GLfloat, GLfloat)
projection axis polygon = (minimum d, maximum d)
    where d = map (\point -> dotProduct point axis) $ points polygon

-- PRIVATE

edgefiy :: [Vector] -> [(Vector, Vector)]
edgefiy [] = []
edgefiy (s:e:[]) = [(s,e)]
edgefiy (s:e:p) = (s, e) : edgefiy (e:p)