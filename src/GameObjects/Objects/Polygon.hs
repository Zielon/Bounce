{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module GameObjects.Objects.Polygon(
    GamePolygon(..),
    GameObject(..),
    GameObject_(..),
    Type(..),
    Vector,
    edgefiy
) where

import Prelude hiding (id)
import Graphics.UI.GLUT     as G
import Data.List
import Text.Printf
import Data.Aeson (ToJSON)
import GHC.Generics

import GameObjects.GameObject
import Common.Drawable
import Collision.VectorOperations

data GamePolygon = GamePolygon {
    id       :: Int,
    velocity :: Vector,
    points   :: [Vector]
} deriving (Show, Generic, ToJSON)

instance GameObject_ GamePolygon where
    setVelocity v polygon = polygon { velocity = v }
    setOffset (x1, y1) polygon = polygon { points = (map (\(x2, y2) -> (x1 + x2, y1 + y2)) p), velocity = (0.0, 0.0) } where p = points polygon
    getEdges polygon = map (\(a, b) -> (-.) b a ) $ (edgefiy p) ++ [(last p, head p)]           where p = points polygon
    getId polygon = id polygon
    getVelocity polygon = velocity polygon
    getPoints polygon = points polygon
    getType polygon = PolygonType
    getHovered polygon = error "Not implemented exception"
    setHovered polygon = error "Not implemented exception"
    getRadius polygon  = error "Not implemented exception"
    getCenter polygon = (totalX/count, totalY/count)
        where p      = points polygon
              count  = realToFrac (length p)
              totalX = foldr (\(x,y) s -> s + x) 0.0 p
              totalY = foldr (\(x,y) s -> s + y) 0.0 p
    draw polygon = preservingMatrix $ do
            renderPrimitive G.Polygon $ mapM_ (\(x, y) -> (getColor3f x y 0.5) >> (vertex $ Vertex3 x y 0)) $ points polygon
            getColor3f (-1) (-1) (-1)
            translate $ Vector3 x y 0
            rasterPos (Vertex2 (0.0::GLfloat) (0.0::GLfloat))
            renderString Helvetica18 $ printf "%d" (id polygon)
        where (x, y) = getCenter polygon

    -- | Projection of each point on the axis to find the length on the perpendicular axis
    --  @axis    - perpendicular vector to a selected axis
    --  @polygon - the given polygon
    --
    projection axis polygon = (minimum d, maximum d) where d = map (\point -> dotProduct point axis) $ points polygon

instance Drawable_ GamePolygon where
    render polygon = draw polygon

instance Eq GamePolygon where
    (==) a b = id a == id b
    (/=) a b = id a /= id b

instance Ord GamePolygon where
    compare a b = (id a) `compare` (id b)
    (<)  a b    = id a <  id b
    (>=) a b    = id a >= id b
    (>)  a b    = id a >  id b
    (<=) a b    = id a <= id b

edgefiy :: [Vector] -> [(Vector, Vector)]
edgefiy [] = []
edgefiy (s:e:[]) = [(s,e)]
edgefiy (s:e:p) = (s, e) : edgefiy (e:p)