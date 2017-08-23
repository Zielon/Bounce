module Collision.RayCasting where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Ratio
import Data.Maybe
import Data.Map                      as M
import Data.List                     as L
import Control.Parallel.Strategies

import Factory.Producer
import API.Ternary
import GameObjects.Objects.Polygon   as P
import GameObjects.Objects.Ball      as B
import GameObjects.Objects.Segment   as S
import Collision.VectorOperations    as O
import GameObjects.GameObject
import Collision.Helpers

type Point = (GLfloat, GLfloat)
type Polygon = [Point]
data Line = Sloped {lineSlope, lineYIntercept :: GLfloat} |
            Vert {lineXIntercept :: GLfloat}
 
polygonSides :: Polygon -> [(Point, Point)]
polygonSides poly@(p1 : ps) = zip poly $ ps ++ [p1]
 
intersectPolygon :: Point -> Line -> Bool
-- @intersectPolygon (px, py) l@ is true if the ray {(x, py) | x ≥ px} intersectPolygon l.
intersectPolygon (px, _)  (Vert xint)  = px <= xint
intersectPolygon (px, py) (Sloped m b) | m < 0     = py <= m * px + b
                                 | otherwise = py >= m * px + b

onLine :: Point -> Line -> Bool
-- Is the point on the line?
onLine (px, _)  (Vert xint)  = px == xint
onLine (px, py) (Sloped m b) = py == m * px + b

carrier :: (Point, Point) -> Line
-- Finds the line containing the given line segment.
carrier ((ax, ay), (bx, by)) | ax == bx  = Vert ax
                             | otherwise = Sloped slope yint
  where slope = (ay - by) / (ax - bx)   -- tan (b/a) = m
        yint = ay - slope * ax          -- y = mx + b -> b = y - mx
 
between :: Ord a => a -> a -> a -> Bool
between x a b | a > b     = b <= x && x <= a
              | otherwise = a <= x && x <= b

inPolygon :: Point -> Polygon -> Bool
inPolygon p @ (px, py) = f 0 . polygonSides
  where f n []                             = odd n
        f n (side : sides) | far           = f n       sides
                           | onSegment     = True
                           | rayintersectPolygon = f (n + 1) sides
                           | otherwise     = f n       sides
          where far = not $ between py ay by
                onSegment | ay == by  = between px ax bx
                          | otherwise = p `onLine` line
                rayintersectPolygon =
                    intersectPolygon p line &&
                    (py /= ay || by < py) &&
                    (py /= by || ay < py)
                ((ax, ay), (bx, by)) = side
                line = carrier side

pointInPolygon :: Int -> Vector -> IORef (Map Int GameObject) -> IO ()
pointInPolygon i mouse ioObjects = do
    objects <- get ioObjects
    case M.lookup i objects of
            Nothing             -> return ()
            Just (GameObject a) -> do
                inPolygon mouse (getPoints a) == True ? 
                    ioObjects ^& (\p -> (GameObject $ setHovered True  a) #- p) :?
                    ioObjects ^& (\p -> (GameObject $ setHovered False a) #- p)

pointInCircle :: Int -> Vector -> IORef (Map Int GameObject) -> IO ()
pointInCircle i mouse ioObjects = do
    objects <- get ioObjects
    case M.lookup i objects of
            Nothing             -> return ()
            Just (GameObject a) -> do
                (x - c_x)^2 + (y - c_y)^2 < r^2 ? 
                    ioObjects ^& (\p -> (GameObject $ setHovered True  a) #- p) :?
                    ioObjects ^& (\p -> (GameObject $ setHovered False a) #- p)
                where r          = getRadius a
                      (c_x, c_y) = getCenter a
                      (x,y)      = mouse

parM :: (a -> b) -> [a] -> Eval [b]
parM f [] = return []
parM f (a:as) = do
    b <- rpar (f a)
    bs <- parM f as
    return (b:bs)

-- Detection that two line segments intersect
--
rayCasting :: IORef (Map Int GameObject) -> IORef [Segment] -> IORef Vector -> IO ()
rayCasting ioObjects segments mouse = do
    objects <- get ioObjects
    m       <- get mouse

    let solve ray    = rays m ray objects
        raysSegments = getSegments 100 m

    segments ^& \s -> runEval (parM solve raysSegments)

    where intersect (p, r) (q, s) =
            if interval t && interval u then i else r
            where e1 = r -. p
                  e2 = s -. q
                  cross = (e1 × e2)
                  t  = (q -. p) × e1 / cross
                  u  = (q -. p) × e2 / cross
                  i = p +. (e1 *. u)
                  interval a = a >= 0 && a <= 1

          intersectPolygon (p, r) []          = []
          intersectPolygon (p, r) [(q, s)]    = [intersect (p, r) (q, s)]
          intersectPolygon (p, r) ((q, s):xs) = [intersect (p, r) (q, s)] ++ intersectPolygon (p, r) xs

          intersectBall (p, r) center radius = dot >= 0 && distance <= radius ? [vector] :? [r]
            where edge     = r -. p
                  axis     = center -. p
                  dot      = axis • (O.normalize edge)
                  vector   = p +. ((O.normalize edge) *. dot)
                  distance = magnitude (center -. vector)

          rays p ray objects = length intersections == 0 ? Segment color p r :? 
                let (_, v) = L.minimumBy (\(l1, _) (l2, _) -> compare l1 l2) (Prelude.map (\e -> (O.lenght e p, e)) intersections) in Segment color p v
            where r = end ray
                  color = lineColor ray
                  intersections = L.foldr (++) [] $ L.map (\(_,(GameObject o)) ->
                      getType o == PolygonType ? intersectPolygon (p, r) (polygonSides (getPoints o)) :?
                                                 intersectBall    (p, r) (getCenter o) (getRadius o)) $ toList objects