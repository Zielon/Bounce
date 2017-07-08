module API.CollisionTests(
    Collision(..),
    floorTestAABB,
    ballTestAABB
) where

import GameObjects.Floor
import GameObjects.Ball

data Collision = AxisX | OverAxisY | UnderAxisY | None deriving Eq

-- AABB test for floors
floorTestAABB :: Floor -> Floor -> Collision
floorTestAABB a b =
    if d1x > 0.0 || d1y > 0.0 then None
    else if d2x > 0.0 || d2y > 0.0 then None
    else OverAxisY
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