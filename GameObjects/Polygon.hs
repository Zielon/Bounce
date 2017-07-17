module GameObjects.Polygon where

import Prelude hiding (id)

import GameObjects.General

data Polygon = Polygon {
    id     :: Int,
    points :: [Point]
}

instance Eq Polygon where
    (==) a b = id a == id b
    (/=) a b = id a /= id b

instance Ord Polygon where
    compare a b = (id a) `compare` (id b)
    (<)  a b    = id a <  id b
    (>=) a b    = id a >= id b
    (>)  a b    = id a >  id b
    (<=) a b    = id a <= id b