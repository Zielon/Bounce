module Collision.AABB(
    Collision(..),
    testAABB,
    gridIntersect2D,
    checkBallCollision
) where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Text.Printf

import API.Ternary
import GameObjects.Objects.Ball
import GameObjects.GameObject
import Collision.VectorOperations

-- AABB ball collision
checkBallCollision :: Ball -> GamePolygon -> Bool
checkBallCollision ball obj = False
    -- lenght (ball_x, ball_y) (nearestX, nearestY) > radius ball ? True :? False
    -- where (ball_x, ball_y)  = getCenter ball
    --       (obj_x, obj_y)    = bottom_left obj
    --       nearestX = clamp ball_x obj_x (obj_x + getWidth obj)
    --       nearestY = clamp ball_y obj_y (obj_y + getHeight obj)