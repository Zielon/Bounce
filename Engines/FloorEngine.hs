module Engines.FloorEngine(
    Floor(Floor, top_left, top_right, bottom_left, id), 
    Point, 
    getFloors, 
    getPoints, 
    moveDownAll,
    moveDownSingle, 
    getMockedFloors) 
where

import Graphics.UI.GLUT
import Prelude hiding (snd, id, floor)
import Data.IORef
import System.Random

import GameObjects.Floor
import API.CollisionTests

getFloors :: [(GLfloat, GLfloat)] -> [Floor]
getFloors list = map (\((x,y), i) -> sfloor i x y) $ zip list [1..]

getPoints :: Floor -> [Point]
getPoints floor = [top_left floor, bottom_left floor, bottom_right floor, top_right floor]

evaluate :: GLfloat -> Floor -> (Point -> Point) -> Floor
evaluate x flr fun =
    if snd tl > -1.0 then Floor (fun tl) (fun tr) (fun bl) (fun br) identifier
    else sfloor identifier x 1.0
    where tl = top_left flr
          bl = bottom_left flr
          br = bottom_right flr
          tr = top_right flr
          identifier = id flr

moveDownSingle :: Floor -> GLfloat -> [Floor] -> [Floor]
moveDownSingle floor y floors = moveDown'' floor 0.0 floors y

moveDownAll ::  GLfloat -> IORef StdGen -> IORef [Floor] -> IO ()
moveDownAll y generator floors = do
    gen     <- get generator
    let (value, newGenerator) = randomR (-1,1) gen
    floors    $~! (\f -> moveDown' value f y)
    generator $~! (\g -> newGenerator)

-- Floor [game element]
-- r - width
-- t - thickness
gfloor :: Int -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Floor
gfloor id x y r t = Floor tl tr bl br id
    where tl = ( x - r, y + t, 0.0)  -- | top left
          bl = ( x - r, y - t, 0.0)  -- | bottom left
          br = ( x + r, y - t, 0.0)  -- | bottom right
          tr = ( x + r, y + t, 0.0)  -- | top right 

sfloor i x y = gfloor i x y width thickness
               where width     = 0.3
                     thickness = 0.025

getMockedFloors :: [(GLfloat, GLfloat)]
getMockedFloors = [(0.5, 0.5), (0.3, -0.4), (0.7, -0.7), (0.7, 0.8), (-0.7, 0.5), (-0.1, 0.8)]

-- | Private section --------------
moveDown' :: GLfloat -> [Floor] -> GLfloat -> [Floor]
moveDown' value floors indicator = map (\f -> evaluate value f $ \(x, y, z) -> (x, y - indicator, z)) floors

moveDown'' :: Floor -> GLfloat -> [Floor] -> GLfloat -> [Floor]
moveDown'' floor value floors indicator =
    map (\f -> if floorTestAABB f floor' == OverAxisY && (id f) /= (id floor') 
               then evaluate value f $ \(x, y, z) -> (x, y - indicator, z) 
               else f)
    $ map (\f -> if (id f) == (id floor) 
                 then evaluate value f $ \(x, y, z) -> (x, y - indicator, z) 
                 else f) floors

    where floor' = evaluate value floor $ \(x, y, z) -> (x, y - indicator, z)
-- | ------------------------------
