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
 
intersects :: Point -> Line -> Bool
{- @intersects (px, py) l@ is true if the ray {(x, py) | x ≥ px} intersects l. -}
intersects (px, _)  (Vert xint)  = px <= xint
intersects (px, py) (Sloped m b) | m < 0     = py <= m * px + b
                                 | otherwise = py >= m * px + b

onLine :: Point -> Line -> Bool
{- Is the point on the line? -}
onLine (px, _)  (Vert xint)  = px == xint
onLine (px, py) (Sloped m b) = py == m * px + b

carrier :: (Point, Point) -> Line
{- Finds the line containing the given line segment. -}
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
                           | rayIntersects = f (n + 1) sides
                           | otherwise     = f n       sides
          where far = not $ between py ay by
                onSegment | ay == by  = between px ax bx
                          | otherwise = p `onLine` line
                rayIntersects =
                    intersects p line &&
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
        raysSegments = getSegments 200 m

    segments ^& \s -> runEval (parM solve raysSegments)

    where intersect (p, r) (q, s) =
            if interval t && interval u then i else (0,0)
            where e1 = (r -. p)   -- mouse -> ray edge
                  e2 = (s -. q)   -- polyon       edge
                  cross = (e1 × e2)
                  t  = (q -. p) × e1 / cross
                  u  = (q -. p) × e2 / cross
                  i = p +. (e1 *. u)
                  interval a = a >= 0 && a <= 1

          intersects (p, r) []          = []
          intersects (p, r) [(q, s)]    = [intersect (p, r) (q, s)]
          intersects (p, r) ((q, s):xs) = L.filter (\e -> e /= (0,0)) $ [intersect (p, r) (q, s)] ++ intersects (p, r) xs

          rays p ray objects = length intersections == 0 ? Segment color p r :? 
                let (_, v) = L.minimumBy (\(l1, _) (l2, _) -> compare l1 l2) (Prelude.map (\e -> (O.lenght e p, e)) intersections) in Segment color p v
            where r = end ray
                  color = lineColor ray
                  intersections = L.foldr (++) [] $ L.map (\(_,(GameObject o)) -> intersects (p, r) $ polygonSides $ getPoints o) $ toList objects