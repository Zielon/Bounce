module API.CollisionTests(
    Collision(..),
    floorTestAABB,
    ballTestAABB,
    gridIntersect2D
) where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (fst, id, lookup, snd, map, Right, Left)
import Data.List        hiding (insert, lookup, map)
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map


import GameObjects.Floor
import GameObjects.Ball
import Engines.FloorEngine

data Collision = Left | Right | Over | Under | X |None
    deriving (Eq, Show)

-- AABB test for floors by a
floorTestAABB :: Floor -> Floor -> Collision
floorTestAABB a b =
    if      d1x > 0.0 || d1y > 0.0 then None
    else if d2x > 0.0 || d2y > 0.0 then None
    else if b_min_y < a_max_y && (d1x < 0.0 || d2x < 0.0) then Under -- a under b
    else if a_min_y < b_max_y && (d1x < 0.0 || d2x < 0.0) then Over  -- a over  b
    else if d2x < d1x && (d1y < 0.0 || d1y < 0.0) then Right -- a on right b
    else if d1x < d2x && (d1y < 0.0 || d1y < 0.0) then Left  -- a on left b
    else None
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
    if      d1x > 0.0 || d1y > 0.0 then None
    else if d2x > 0.0 || d2y > 0.0 then None
    else if b_max_y - a_min_y > 0.0 && a_max_y > b_max_y && (d1x < 0.0 || d2x < 0.0) then Over  -- a over  b
    else if a_max_y - b_min_y > 0.0 && a_min_y < b_min_y && (d1x < 0.0 || d2x < 0.0) then Under -- a under b
    else if a_min_x < b_max_x && (d1y < 0.0 || d1y < 0.0) then Right -- a on right b
    else if a_max_x < b_min_x && (d1y < 0.0 || d1y < 0.0) then Left  -- a on left b
    else None
    where radius = 0.05
          edge   = 0.025
          (x,y)  = getPosition ball
          (a_min_x, a_min_y, _) = ((x-radius), (y-radius), 0)
          (a_max_x, a_max_y, _) = ((x + radius), (y + radius), 0)
          (b_min_x, b_min_y, _) = bottom_left floor
          (b_max_x, b_max_y, _) = top_right floor
          d1x = b_min_x - a_max_x
          d1y = b_min_y - a_max_y
          d2x = a_min_x - b_max_x
          d2y = a_min_y - b_max_y

--                        (minX,maxX)         (minY,maxY)
getMinMax :: Floor -> ((GLfloat, GLfloat), (GLfloat,GLfloat))
getMinMax floor = ((GameObjects.Floor.fst $ bottom_left floor, GameObjects.Floor.fst $ top_right floor), 
                   (GameObjects.Floor.snd $ bottom_left floor, GameObjects.Floor.snd $ top_right floor))

gridIntersect2D :: IORef (Map Int Floor)-> IO ()
gridIntersect2D dictionary = do
    floors <- get dictionary
    grid   <- newIORef $ (fromList [] :: Map (Int, Int) [Int])
    forM_ floors $ \a -> do
        g  <- get grid
        let i = (id a)
        let ((a_min_x, a_max_x), (a_min_y, a_max_y)) = getMinMax a
        forM_ [Prelude.floor(a_min_x/h)..Prelude.ceiling(a_max_x/h)] $ \x ->
            forM_ [Prelude.floor(a_min_y/h)..Prelude.ceiling(a_max_y/h)] $ \y -> do
                case lookup (x,y) g of
                    Nothing    -> grid $~! (\d -> insertWith (++) (x,y) [i] d)    -- Not exist
                    Just cells ->
                        do grid $~! (\d -> insertWith (++) (x,y) [i] d)           -- Update the list on the (x,y) position in the map
                           forM_ cells $ \n -> do
                             case lookup n floors of
                                Nothing -> return ()
                                Just b  -> do
                                        putStrLn $ printf "[a : %d] [b : %d] = %s" (id a) (id b) (show $ floorTestAABB a b)
                                        case floorTestAABB a b of
                                            Over  -> dictionary $~! (\d -> insert (id b) (setY (a_min_y - 0.05) (a_min_y) b) d)
                                            Under -> dictionary $~! (\d -> insert (id b) (setY (a_max_y) (a_max_y + 0.05) b) d)
                                            _     -> return ()
    where h = 0.01 :: GLfloat