module API.CollisionTests(
    Collision(..),
    floorTestAABB,
    ballTestAABB,
    gridIntersect2D
) where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (fst, id)
import Data.IORef
import GameObjects.Floor
import GameObjects.Ball
import Control.Monad
import Text.Printf

import Engines.FloorEngine

data Collision = AxisX | OverAxisY | UnderAxisY | None deriving Eq

-- AABB test for floors
floorTestAABB :: Floor -> Floor -> Bool
floorTestAABB a b =
    if d1x > 0.0 || d1y > 0.0 then False
    else if d2x > 0.0 || d2y > 0.0 then False
    else True
    where (a_min_x, a_min_y, _) = bottom_left a
          (a_max_x, a_max_y, _) = top_right a
          (b_min_x, b_min_y, _) = bottom_left b
          (b_max_x, b_max_y, _) = top_right b
          d1x = b_min_x - a_max_x
          d1y = b_min_y - a_max_y
          d2x = a_min_x - b_max_x
          d2y = a_min_y - b_max_y

-- AABB test for the ball
ballTestAABB :: Ball -> Floor -> Collision
ballTestAABB ball floor =
    if d1x > 0.0 || d1y > 0.0 then None
    else if d2x > 0.0 || d2y > 0.0 then None
    else if min_x <= x + edge && max_x >= x - edge then if d1y > d2y then OverAxisY else UnderAxisY
    else if max_y > y + edge && min_y < y - edge then AxisX
    else None
    where radius = 0.05
          edge   = 0.025
          (x,y)  = getPosition ball
          (min_x, min_y, _) = bottom_left floor
          (max_x, max_y, _) = top_right floor
          d1x = (x-radius) - max_x
          d1y = (y-radius) - max_y
          d2x = min_x - (x + radius)
          d2y = min_y - (y + radius)

--                             X                   Y
getMinMax :: Floor -> ((GLfloat, GLfloat), (GLfloat,GLfloat))
getMinMax floor = ((GameObjects.Floor.fst $ bottom_left floor, GameObjects.Floor.fst $ top_right floor), 
                   (GameObjects.Floor.snd $ bottom_left floor, GameObjects.Floor.snd $ top_right floor))

getFloorByIndex :: [Floor] -> Int -> Floor
getFloorByIndex floors i = filter (\f -> (id f) == i) floors !! 0

gridIntersect2D :: IORef [Floor] -> IO ()
gridIntersect2D ioFloors = do
    floors <- get ioFloors
    grid   <- newIORef []
    forM_ floors $ \f -> do
        g <- get grid
        let i = (id f)
        let ((min_x, max_x), (min_y, max_y)) = getMinMax f
        forM_ [Prelude.floor(min_x/h)..Prelude.ceiling(max_x/h)] $ \x ->
            forM_ [Prelude.floor(min_y/h)..Prelude.ceiling(max_y/h)] $ \y -> do
                let cells = filter (\((a,b), _) -> (a,b) == (x,y)) g
                if cells /= [] then do
                    forM_ (Prelude.snd (cells !! 0)) $ \n -> do
                        let collitionWith = getFloorByIndex floors n
                        let ((c_min_x, c_max_x), (c_min_y, c_max_y)) = getMinMax collitionWith
                        let maxMinY = max c_min_y min_y
                        let maxMinX = max c_min_x min_x
                        if floorTestAABB f collitionWith && Prelude.floor(maxMinX/h) /= x && Prelude.floor(maxMinY/h) /= y
                        then ioFloors $~! (\list ->  map (\e -> if (id e) == (id collitionWith) then setY (min_y - 0.05) (min_y) collitionWith else e ) list)
                        else return ()
                else grid $~! (\g -> g ++ [((x,y), [i])])
    where h = 0.01 :: GLfloat