module Collision.RayCasting where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Ratio
import Data.Map                      as M

import API.Ternary
import GameObjects.Objects.Polygon   as P
import GameObjects.Objects.Ball      as B
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
  where slope = (ay - by) / (ax - bx)
        yint = ay - slope * ax
 
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
                inPolygon mouse (getPoints a) == True ? ioObjects ^& (\p -> (GameObject $ setHovered True  a) #- p) :?
                                                        ioObjects ^& (\p -> (GameObject $ setHovered False a) #- p)

pointInCircle :: Int -> Vector -> IORef (Map Int GameObject) -> IO ()
pointInCircle i mouse ioObjects = do
    objects <- get ioObjects
    case M.lookup i objects of
            Nothing             -> return ()
            Just (GameObject a) -> do
                (x - c_x)^2 + (y - c_y)^2 < r^2 ? ioObjects ^& (\p -> (GameObject $ setHovered True  a) #- p) :?
                                                  ioObjects ^& (\p -> (GameObject $ setHovered False a) #- p)
                where r          = getRadius a
                      (c_x, c_y) = getCenter a
                      (x,y)      = mouse