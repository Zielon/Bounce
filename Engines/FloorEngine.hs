module Engines.FloorEngine(
    Floor(Floor, top_left, top_right, bottom_left, id), 
    Point, 
    getFloors, 
    getPoints, 
    moveDownAll,
    moveSingle, 
    getMockedFloors) 
where

import Prelude   hiding (id, floor)
import Data.List hiding(insert)
import Graphics.UI.GLUT
import Data.IORef
import Data.Map
import System.Random

import GameObjects.Floor

getFloors :: [(GLfloat, GLfloat)] -> (Map Int Floor)
getFloors list = fromList $ Prelude.map (\((x,y), i) -> (i, sfloor i x y)) $ zip list [1..]

getPoints :: Floor -> [Point]
getPoints floor = [top_left floor, bottom_left floor, bottom_right floor, top_right floor]

moveSingle :: Floor -> GLfloat -> (Map Int Floor) -> (Map Int Floor)
moveSingle floor y floors = moveDown'' floor 0.0 floors y

moveDownAll ::  GLfloat -> IORef StdGen -> IORef (Map Int Floor) -> IO ()
moveDownAll y generator floors = do
    gen     <- get generator
    let (value, newGenerator) = randomR (-1,1) gen
    floors    $~! (\f -> moveDown' value f y)
    generator $~! (\g -> newGenerator)

-- Floor [game element]
-- r - width
-- t - height
gfloor :: Int -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Floor
gfloor id x y r t = Floor x y tl tr bl br id color (r*2) (t*2)
    where tl = ( x - r, y + t)  -- | top left
          bl = ( x - r, y - t)  -- | bottom left
          br = ( x + r, y - t)  -- | bottom right
          tr = ( x + r, y + t)  -- | top right
          color = (((x+1)/2), ((y+1)/2), ((0+1)/2))

-- | Standard floor
--
sfloor i x y = gfloor i x y width height
               where width  = 0.3
                     height = 0.025

-- | Private section
--
moveDown' :: GLfloat -> (Map Int Floor) -> GLfloat -> (Map Int Floor)
moveDown' random floors indicator = Data.Map.map (\f -> evaluate random f $ \(x, y) -> (x, y - indicator)) floors

moveDown'' :: Floor -> GLfloat -> (Map Int Floor) -> GLfloat -> (Map Int Floor)
moveDown'' floor random floors indicator = insert (id floor) (evaluate random floor $ \(x, y) -> (x, y - indicator)) floors

evaluate :: GLfloat -> Floor -> (Point -> Point) -> Floor
evaluate random floor fun =
    if snd tl > -1.0 then Floor x' y' (fun tl) (fun tr) (fun bl) (fun br) identifier colors (width floor) (height floor)
    else sfloor identifier random 1.0
    where identifier = id floor
          colors     = color3f floor
          tl         = top_left floor
          bl         = bottom_left floor
          br         = bottom_right floor
          tr         = top_right floor
          x'         = x floor 
          y'         = y floor

-- | Mock section
--
getMockedFloors :: [(GLfloat, GLfloat)]
getMockedFloors = [(0.1, 0.5), (-0.3, 0.8), (-0.2, 0.2), (-0.3, 0.7), (-0.1, 0.3), (0.0, 0.4)]