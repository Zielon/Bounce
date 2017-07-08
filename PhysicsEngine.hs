{-# LANGUAGE FlexibleContexts #-}

module PhysicsEngine (
    updateGravity, 
    collisionBoundaries, 
    collisionEdges) 
where

import Graphics.UI.GLUT
import Control.Monad    
import Data.IORef
import Control.Concurrent
import Text.Printf
import Prelude hiding (id)

import FloorEngine
import Ball

updateGravity :: IORef Ball -> Float -> IO ()
updateGravity ball dt = do
    ball $~! \b -> setVelocity b $ \(vX,vY) -> (vX, (vY + acc * dt))
    ball' <- get ball
    let (vX, vY) = getVelocity ball'
    ball $~! \b -> setPosition b $ \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2))
    where acc = -9.80665

earth :: Float -> Float
earth v = v * (ball - earth) / (ball + earth)
          where ball  = 50.0
                earth = 500.0

collisionBoundaries :: IORef Ball -> IO ()
collisionBoundaries ball = do
    ball' <- get ball
    let (x,y) = getPosition ball'
    when (y < -0.95) $ ball $~! \b -> setVelocity b $ \(vX,vY) -> (vX, earth vY)
    when (x > 0.95  || x < -0.95) $ ball $~! \b -> setVelocity b $ \(vX,vY) -> (earth vX, vY)
    -- Move back the ball when overstep the boundaries
    when (y < -0.95) $ ball $~! \b -> setPosition b $ \(x,y) -> (x, -0.95)
    --when (y > 0.95 ) $ pos $~! (\(x',y') -> (x', 0.95))
    when (x > 0.95 ) $ ball $~! \b -> setPosition b $ \(x,y) -> (0.95, y)
    when (x < -0.95) $ ball $~! \b -> setPosition b $ \(x,y) -> (-0.95, y)

-- AABB test
collisionEdges :: IORef Ball -> IORef [Floor] -> IO ()
collisionEdges ball floors = do
    fls <- get floors
    forM_ fls $ \f -> do
        ball' <- get ball
        let (x,y) = getPosition ball'
        let (min_x, min_y, _) = bottom_left f
        let (max_x, max_y, _) = top_right f
        let d1x = (x-radius) - max_x
        let d1y = (y-radius) - max_y
        let d2x = min_x - (x + radius)
        let d2y = min_y - (y + radius)
        -- Check AABB test
        if d1x > 0.0 || d1y > 0.0 then return ()
        else if d2x > 0.0 || d2y > 0.0 then return ()
        -- Collision occured
        else if min_x <= x + edge && max_x >= x - edge then do
            (ball $~! \b -> updateScore b (id f)) 
            >> (ball $~! \b -> setVelocity b $ \(vX,vY) -> (vX, earth vY)) 
            >> (if d1y > d2y 
                then floors $~! (\floor' -> moveDownSingle (id f) (abs (y - radius - max_y)) floor')
                else floors $~! \floor' -> moveDownSingle (id f) (-(abs (y + radius - min_y))) floor') -- under
        else if max_y > y + edge && min_y < y - edge then ball $~! \b -> setVelocity b $ \(vX,vY) -> (earth vX, vY)
        else return ()
        where radius = 0.05
              edge = 0.025