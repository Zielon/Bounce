{-# LANGUAGE FlexibleContexts #-}

module Physics where

import Graphics.UI.GLUT
import Control.Monad    
import Data.IORef
import Control.Concurrent

import FloorGenerator

updateGravity :: IORef GLfloat -> 
                 IORef GLfloat ->  
                 [Floor] -> 
                 IORef (GLfloat, GLfloat) -> IO ()
updateGravity velocityX velocityY edges pos = do
    -- Check collision with map elements
    collisionBoundaries velocityY velocityX pos
    collisionEdges      velocityY velocityX edges pos
    
    vX <- get velocityX
    velocityY $~! \v -> v + acc * dt
    vY <- get velocityY

    when (vY > 0.002 || vY < -0.002) (pos $~! \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2)))

    where acc = -9.80665
          dt  = 0.0005

earth :: Float -> Float
earth v = v * (ball - earth) / (ball + earth)
          where ball  = 50.0
                earth = 500.0

collisionBoundaries :: IORef GLfloat -> 
                       IORef GLfloat ->  
                       IORef (GLfloat, GLfloat) -> IO ()
collisionBoundaries velocityY velocityX pos = do
    (x,y) <- get pos
    when (y < -0.90) (velocityY $~! \v -> earth v)
    when (y > 0.95 ) (velocityY $~! \v -> v - 0.05)
    when (x > 0.95 ) (velocityX $~! \v -> v - 0.05) 
    when (x < -0.95) (velocityX $~! \v -> v + 0.05)

collisionEdges :: IORef GLfloat -> 
                  IORef GLfloat -> 
                  [Floor] ->
                  IORef (GLfloat, GLfloat) -> IO ()
collisionEdges velocityY velocityX floors pos = do
    (x,y) <- get pos

    vY <- get velocityY
    vX <- get velocityX

    let ball  = 0.04
    let is    = \y line -> y >= line - 0.009 && y <= line + 0.009

    let over  = \top    -> when (is (y - ball) top) (velocityY $~! \v -> earth v)
    let under = \bottom -> when (is (y + ball) bottom) (velocityY $~! \v -> v - 0.05)

    forM_ floors $ \f -> do
        
        let (tl_x, tl_y, _) = top_left f
        let (tr_x, tr_y, _) = top_right f
        let (br_x, br_y, _) = bottom_right f

        if tl_x <= x && tr_x >= x then over tl_y >> under br_y
        else return ()