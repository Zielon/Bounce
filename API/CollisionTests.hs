module API.CollisionTests(
    Collision(..),
    floorTestAABB,
    ballTestAABB,
    gridIntersect2D
) where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (fst, id, lookup, snd)
import Data.List        hiding (insert, lookup)
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map


import GameObjects.Floor
import GameObjects.Ball
import Engines.FloorEngine

data Collision = AxisX | OverAxisY | UnderAxisY | None 
    deriving Eq

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

--                        (minX,maxX)         (minY,maxY)
getMinMax :: Floor -> ((GLfloat, GLfloat), (GLfloat,GLfloat))
getMinMax floor = ((GameObjects.Floor.fst $ bottom_left floor, GameObjects.Floor.fst $ top_right floor), 
                   (GameObjects.Floor.snd $ bottom_left floor, GameObjects.Floor.snd $ top_right floor))

gridIntersect2D :: IORef (Map Int Floor)-> IO ()
gridIntersect2D dictionary = do
    floors <- get dictionary
    grid   <- newIORef $ (fromList [] :: Map (Int, Int) [Int])
    let sorted = sortBy (\(_, f1) (_, f2) -> snd(bottom_left f1) `compare` snd(bottom_left f2)) $ toList floors -- Max 6 elements
    forM_ sorted $ \(_, f) -> do
        g  <- get grid
        let i = (id f)
            ((min_x, max_x), (min_y, max_y)) = getMinMax f
        forM_ [Prelude.floor(min_x/h)..Prelude.ceiling(max_x/h)] $ \x ->
            forM_ [Prelude.floor(min_y/h)..Prelude.ceiling(max_y/h)] $ \y -> do
                case lookup (x,y) g of
                    Nothing    ->    grid $~! (\d -> insertWith (++) (x,y) [i] d) -- Not exist
                    Just cells -> do grid $~! (\d -> insertWith (++) (x,y) [i] d) -- Update the list on the (x,y) position in the map
                                     forM_ cells $ \n -> do
                                        case lookup n floors of
                                            Nothing            -> return ()
                                            Just collisionWith -> do
                                                let ((c_min_x, c_max_x), (c_min_y, c_max_y)) = getMinMax collisionWith
                                                    maxMinY = max c_min_y min_y
                                                    maxMinX = max c_min_x min_x
                                                if floorTestAABB f collisionWith && Prelude.floor(maxMinX/h) /= x 
                                                                                 && Prelude.floor(maxMinY/h) /= y
                                                then dictionary $~! (\d -> insert (id collisionWith) (setY (min_y - 0.05) (min_y) collisionWith) d)
                                                else return ()
    where h = 0.05 :: GLfloat