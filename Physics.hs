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
    (velocityY $~! \v -> v + acc * dt)
    vY <- get velocityY
    pos $~! \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2))
    postRedisplay Nothing
    where acc = -9.80665
          dt  = 0.0005

collisionBoundaries :: IORef GLfloat -> 
                       IORef GLfloat ->  
                       IORef (GLfloat, GLfloat) -> IO ()
collisionBoundaries velocityY velocityX pos = do
    (x,y) <- get pos
    when (y < -0.90) (velocityY $~! \v -> v * (ball - earth) / (ball + earth))
    when (y > 0.95 ) (velocityY $~! \v -> v - 0.05)
    when (x > 0.95 ) (velocityX $~! \v -> v - 0.05) 
    when (x < -0.95) (velocityX $~! \v -> v + 0.05)
    where ball  = 50.0
          earth = 500.0

collisionEdges :: IORef GLfloat -> 
                  IORef GLfloat -> 
                  [Floor] ->
                  IORef (GLfloat, GLfloat) -> IO ()
collisionEdges velocityY velocityX floors pos = do
    (x,y) <- get pos
    forM_ floors $ \f -> do
        vY <- get velocityY
        postRedisplay Nothing
        where ball      = 50.0
              earth     = 500.0