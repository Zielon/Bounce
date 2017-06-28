{-# LANGUAGE FlexibleContexts #-}

module Physics where

import Graphics.UI.GLUT
import Control.Monad    
import Data.IORef
import Control.Concurrent

import FloorGenerator

updateGravity :: IORef GLfloat -> 
                 IORef GLfloat ->  
                 IORef [Floor] -> 
                 IORef (GLfloat, GLfloat) -> IO ()
updateGravity velocityX velocityY edges pos = do
    -- Check collision with map elements
    collisionBoundaries velocityY velocityX pos
    collisionEdges      velocityY velocityX edges pos

    vX <- get velocityX
    velocityY $~! \v -> v + acc * dt
    vY <- get velocityY
    pos $~! \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2))
    where acc = -9.80665
          dt  = 0.0005

earth :: Float -> Float
earth v = v * (ball - earth) / (ball + earth)
          where ball  = 5.0
                earth = 500.0

collisionBoundaries :: IORef GLfloat -> 
                       IORef GLfloat ->  
                       IORef (GLfloat, GLfloat) -> IO ()
collisionBoundaries velocityY velocityX pos = do
    (x,y) <- get pos
    when (y < -0.90) (velocityY $~! \v -> earth v)
    when (y > 0.90 ) (velocityY $~! \v -> earth v)
    when (x > 0.95 ) (velocityX $~! \v -> v - 0.05) 
    when (x < -0.95) (velocityX $~! \v -> v + 0.05)

collisionEdges :: IORef GLfloat -> 
                  IORef GLfloat -> 
                  IORef [Floor] ->
                  IORef (GLfloat, GLfloat) -> IO ()
collisionEdges velocityY velocityX floors pos = do

    fls <- get floors

    let ball  = 0.05
    let is    = \point line -> point >= line - 0.0025 && point <= line + 0.0025
    let check = \first second velocity x y change -> if is (y - ball) first || is (y + ball) second 
                                                     then (velocity $~! \v -> change v)
                                                     else return ()
    forM_ fls $ \f -> do
        
        (x,y) <- get pos

        let (tl_x, tl_y, _) = top_left f
        let (tr_x, tr_y, _) = top_right f
        let (br_x, br_y, _) = bottom_right f

        when (tl_x <= x && tr_x >= x) $ check tl_y br_y velocityY x y earth
        --when (br_y <= y && tr_y >= y) $ check tl_x br_x velocityX x y $ \v -> v - 0.05