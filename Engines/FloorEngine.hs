module Engines.FloorEngine(
    Floor(Floor, top_left, top_right, bottom_left, id), 
    Point, 
    getFloors, 
    getPoints, 
    moveDownAll,
    moveDownSingle, 
    getMockedFloors) 
where

import Prelude hiding (snd, id, floor)
import Graphics.UI.GLUT
import Data.IORef
import Data.Map
import System.Random


import GameObjects.Floor

getFloors :: [(GLfloat, GLfloat)] -> (Map Int Floor)
getFloors list = fromList $ Prelude.map (\((x,y), i) -> (i, sfloor i x y)) $ zip list [1..]

getPoints :: Floor -> [Point]
getPoints floor = [top_left floor, bottom_left floor, bottom_right floor, top_right floor]

evaluate :: GLfloat -> Floor -> (Point -> Point) -> Floor
evaluate x flr fun =
    if snd tl > -1.0 then Floor (fun tl) (fun tr) (fun bl) (fun br) identifier colors
    else sfloor identifier x 1.0
    where tl = top_left flr
          bl = bottom_left flr  
          br = bottom_right flr
          tr = top_right flr
          identifier = id flr
          colors = color3f flr

moveDownSingle :: Floor -> GLfloat -> (Map Int Floor) -> (Map Int Floor)
moveDownSingle floor y floors = moveDown'' floor 0.0 floors y

moveDownAll ::  GLfloat -> IORef StdGen -> IORef (Map Int Floor) -> IO ()
moveDownAll y generator floors = do
    gen     <- get generator
    let (value, newGenerator) = randomR (-1,1) gen
    floors    $~! (\f -> moveDown' value f y)
    generator $~! (\g -> newGenerator)

-- Floor [game element]
-- r - width
-- t - thickness
gfloor :: Int -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Floor
gfloor id x y r t = Floor tl tr bl br id color
    where tl = ( x - r, y + t, 0.0)  -- | top left
          bl = ( x - r, y - t, 0.0)  -- | bottom left
          br = ( x + r, y - t, 0.0)  -- | bottom right
          tr = ( x + r, y + t, 0.0)  -- | top right
          color = (((x+1)/2), ((y+1)/2), ((0+1)/2))

-- | Standard floor
--
sfloor i x y = gfloor i x y width thickness
               where width     = 0.3
                     thickness = 0.025

-- | Private section
--
moveDown' :: GLfloat -> (Map Int Floor) -> GLfloat -> (Map Int Floor)
moveDown' random floors indicator = Data.Map.map (\f -> evaluate random f $ \(x, y, z) -> (x, y - indicator, z)) floors

moveDown'' :: Floor -> GLfloat -> (Map Int Floor) -> GLfloat -> (Map Int Floor)
moveDown'' floor random floors indicator = insert (id floor) (evaluate random floor $ \(x, y, z) -> (x, y - indicator, z)) floors

-- | Mock section
--
getMockedFloors :: [(GLfloat, GLfloat)]
getMockedFloors = [(-0.2, 0.2), (-0.1, 0.3), (0.0, 0.4), (-0.3, 0.1), (0.1, 0.5)]


