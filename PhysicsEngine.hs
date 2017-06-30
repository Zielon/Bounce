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
                 Float ->
                 IO ()
updateGravity velocityX velocityY pos dt = do
    vX <- get velocityX
    velocityY $~! \v -> v + acc * dt
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
    -- Egde values when velocity is weaker
    when (y < -0.95) $ pos $~! (\(x',y') -> (x', -0.95))
    when (y > 0.95 ) $ pos $~! (\(x',y') -> (x', 0.95))
    when (x > 0.95 ) $ pos $~! (\(x',y') -> (0.95, y'))
    when (x < -0.95) $ pos $~! (\(x',y') -> (-0.95, y'))

collisionEdges :: IORef GLfloat -> 
                  IORef GLfloat -> 
                  IORef [Floor] ->
                  IORef (GLfloat, GLfloat) -> IO ()
collisionEdges velocityY velocityX floors pos = do
    fls <- get floors
    forM_ fls $ \f -> do
        (x,y) <- get pos
        when (testAABBOverlap f x y) $ putStrLn $ printf "Collision | x -> %.8f | y -> %.8f" x y

testAABBOverlap :: Floor -> GLfloat -> GLfloat -> Bool
testAABBOverlap f x y = 
    if d1x > 0.0 || d1y > 0.0 then False
    else if d2x > 0.0 || d2y > 0.0 then False
    else True
    where ball = 0.05
          (min_x, min_y, _) = bottom_left f
          (max_x, max_y, _) = top_right f
          d1x = (x-ball) - max_x
          d1y = (y-ball) - max_y
          d2x = min_x - (x + ball)
          d2y = min_y - (y + ball)
