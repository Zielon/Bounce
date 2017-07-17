module GameObjects.Polygon(
    GamePolygon(..),
    Polygonable(..),
    Point,
    projection
) where

import Prelude hiding (id)
import Graphics.UI.GLUT

import GameObjects.Global
import Collision.Helpers

class Polygonable a where
    getEdges :: a -> [Point]

data GamePolygon = GamePolygon {
    id     :: Int,
    points :: [Point]
}

instance Polygonable GamePolygon where
    getEdges polygon = map (\(a, b) -> b -. a ) $ (edgefiy p) ++ [(head p, last p)]
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
    where d = map (\point -> dotProduct axis point) $ points polygon


-- PRIVATE

edgefiy :: [Point] -> [(Point, Point)]
edgefiy [] = []
edgefiy (s:e:[]) = [(s,e)]
edgefiy (s:e:p) = (s, e) : edgefiy (e:p)