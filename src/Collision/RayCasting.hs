module Collision.RayCasting where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Ratio
import Data.Map                      as M
import Data.List                     as L

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

rayCasting :: IORef (Map Int GameObject) -> IORef [Segment] -> IORef Vector -> IO ()
rayCasting ioObjects segments mouse = do
    objects    <- get ioObjects
    mouseStart <- get mouse
    rays       <- get segments
    output     <- newIORef []

    segments ^& \l -> getSegments 10 mouseStart

    forM_ rays $ \ray -> do
        intersections <- newIORef []
        let p = mouseStart
            r = end ray
        forM_ objects $ \(GameObject o) -> do 
            forM_ (polygonSides $ getPoints o) $ \(q, s) -> do
                let t = (q -. p) × s / (r × s)
                    u = (q -. p) × r / (r × s)
                    i = p +. (r *. t)
                if r × s == 0 && (q -. p) × r /= 0 then return ()
                else if r × s /= 0 && interval t && interval u then intersections ^& \l -> l ++ [i]
                else return ()

        list <- get intersections
        if length list == 0 then output ^& \l -> l ++ [Segment (lineColor ray) p r]
        else do
            let (_,v) = L.minimumBy (\(l1, _) (l2, _) -> compare l2 l1) $ Prelude.map (\e -> (O.lenght e p, e)) list
            output ^& \l -> l ++ [Segment (lineColor ray) p v]

    o <- get output
    segments ^& \s -> o
    where interval a = a >= 0 && a <= 1