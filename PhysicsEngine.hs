{-# LANGUAGE FlexibleContexts #-}

module PhysicsEngine (updateGravity, collisionBoundaries, collisionEdges) where

import Graphics.UI.GLUT
import Control.Monad    
import Data.IORef
import Control.Concurrent
import Text.Printf

import FloorGenerator

updateGravity :: IORef GLfloat -> 
                 IORef GLfloat ->  
                 IORef (GLfloat, GLfloat) ->
                 Float -> IO ()
updateGravity velocityX velocityY pos dt = do
    velocityY $~! \v -> v + acc * dt
    vX <- get velocityX 
    vY <- get velocityY
    pos $~! \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2))
    where acc = -9.80665

earth :: Float -> Float
earth v = v * (ball - earth) / (ball + earth)
          where ball  = 50.0
                earth = 500.0

collisionBoundaries :: IORef GLfloat -> 
                       IORef GLfloat ->  
                       IORef (GLfloat, GLfloat) -> IO ()
collisionBoundaries velocityY velocityX pos = do
    (x,y) <- get pos
    when (y < -0.95 || y > 0.95) $ velocityY $~! \v -> earth v
    when (x > 0.95  || x < -0.95) $ velocityX $~! \v -> earth v
    -- Move back the ball when overstep the boundaries
    when (y < -0.95) $ pos $~! (\(x',y') -> (x', -0.95))
    when (y > 0.95 ) $ pos $~! (\(x',y') -> (x', 0.95))
    when (x > 0.95 ) $ pos $~! (\(x',y') -> (0.95, y'))
    when (x < -0.95) $ pos $~! (\(x',y') -> (-0.95, y'))

-- AABB test
collisionEdges :: IORef GLfloat -> 
                  IORef GLfloat -> 
                  IORef [Floor] ->
                  IORef (GLfloat, GLfloat) -> IO ()
collisionEdges velocityY velocityX floors pos = do
    fls <- get floors
    forM_ fls $ \f -> do
        (x,y) <- get pos 
        let (min_x, min_y, _) = bottom_left f
        let (max_x, max_y, _) = top_right f
        let d1x = (x-ball) - max_x
        let d1y = (y-ball) - max_y
        let d2x = min_x - (x + ball)
        let d2y = min_y - (y + ball)
        if d1x > 0.0 || d1y > 0.0 then return ()
        else if d2x > 0.0 || d2y > 0.0 then return ()
        else if max_x > x + ball && min_x < x - ball then do
               if d1y > d2y then (pos $~! (\(x',y') -> (x', max_y + ball))) >> velocityY $~! \v -> earth v
               else (pos $~! (\(x',y') -> (x', min_y - ball))) >> velocityY $~! \v -> earth v
        else do
            if d1x > d2x then (pos $~! (\(x',y') -> (max_x + ball, y'))) >> velocityX $~! \v -> earth v
            else (pos $~! (\(x',y') -> (min_x - ball, y'))) >> velocityX $~! \v -> earth v
        where ball = 0.05