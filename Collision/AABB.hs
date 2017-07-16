{-# LANGUAGE FlexibleContexts #-}

module Collision.AABB(
    Collision(..),
    testAABB,
    gridIntersect2D,
    checkBallCollision
) where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (fst, id, lookup, snd, map, Right, Left)
import Data.List        hiding (insert, lookup, map)
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map as M
import Data.Set as S

import GameObjects.Positionable
import GameObjects.Floor
import GameObjects.Ball

import Engines.FloorEngine

import Collision.Helpers

data Collision = Left | Right | Top | Under | None
    deriving (Eq, Show)

-- AABB test for game AABB objects
testAABB :: (Positionable a) => a -> a -> Collision
testAABB a b =
    if a == b then None
    else if d1x > 0.0 || d1y > 0.0 then None
    else if d2x > 0.0 || d2y > 0.0 then None
    else if a_min_y < b_max_y then Top   -- a over  b
    else Under
    where (a_min_x, a_min_y) = getMin a
          (a_max_x, a_max_y) = getMax a
          (b_min_x, b_min_y) = getMin b
          (b_max_x, b_max_y) = getMax b
          d1x = b_min_x - a_max_x
          d1y = b_min_y - a_max_y
          d2x = a_min_x - b_max_x
          d2y = a_min_y - b_max_y

-- AABB ball collision
checkBallCollision :: Ball -> Floor -> Collision
checkBallCollision ball obj =
    if lenght (ball_x, ball_y) (nearestX, nearestY) > radius ball then None
    else if ball_y > nearestY then Top
    else if ball_y < nearestY then Under
    else if ball_x < nearestX then Left
    else if ball_x > nearestX then Right
    else None

    where (ball_x, ball_y)  = getPosition ball
          (obj_x, obj_y) = bottom_left obj
          nearestX = clamp ball_x obj_x (obj_x + getWidth obj)
          nearestY = clamp ball_y obj_y (obj_y + getHeight obj)

gridIntersect2D :: IORef (Map Int Floor) -> IO ()
gridIntersect2D dictionary = do
    floors <- get dictionary
    grid   <- newIORef $ (M.fromList [] :: Map (Int, Int) [Int])
    forM_  (sortBy (\(_, a) (_, b) -> getY a `compare` getY b) $ M.toList floors) $ \(_, a) -> do
        let i = (id a)
            (a_min_x, a_min_y) = getMin a
            (a_max_x, a_max_y) = getMax a
            xRange = [Prelude.floor(a_min_x/h)..Prelude.ceiling(a_max_x/h)]
            yRange = [Prelude.floor(a_min_y/h)..Prelude.ceiling(a_max_y/h)]
        forM_ xRange $ \x ->
            forM_ yRange $ \y -> do
                grid $~! (\d -> insertWith (++) (x,y) [i] d)
                g  <- get grid
                case lookup (x,y) g of
                    Nothing    -> return ()
                    Just cells -> do forM_ cells $ \n -> do
                                        case lookup n floors of
                                            Nothing -> error "Wrong floor's ID"
                                            Just b  -> action a b
    where h = 0.1 :: GLfloat
          action :: Floor -> Floor -> IO ()
          action a b = do
                let (_, min_y) = getMin a
                    (_, max_y) = getMax a
                case testAABB a b of
                    Top   -> dictionary $~! (\d -> M.insert (id b) (setY (min_y - 0.05) (min_y) b) d)
                    _     -> return ()