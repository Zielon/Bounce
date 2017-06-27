{-# LANGUAGE FlexibleContexts #-}

module Gravity where

import Graphics.UI.GLUT
import Control.Monad    
import Data.IORef
import Control.Concurrent

updateGravity :: IORef GLfloat -> 
                 IORef GLfloat ->  
                 [((GLfloat, GLfloat, GLfloat),(GLfloat, GLfloat, GLfloat))] -> 
                 IORef (GLfloat, GLfloat) -> IO ()
updateGravity velocityX velocityY map pos = do
    vX <- get velocityX
    (velocityY $~! \v -> v + acc * dt)
    vY <- get velocityY
    pos $~! \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2))
    collision velocityY velocityX map pos
    postRedisplay Nothing
    where acc = -9.80665
          dt  = 0.0005

collision :: IORef GLfloat -> 
             IORef GLfloat ->  
             [((GLfloat, GLfloat, GLfloat),(GLfloat, GLfloat, GLfloat))] ->  
             IORef (GLfloat, GLfloat) -> IO ()
collision velocityY velocityX map pos = do
    (x,y) <- get pos
    -- The map boundaries
    when (y < -0.90) (velocityY $~! \v -> v * (ball - earth) / (ball + earth)) >> return ()
    when (y > 0.95 ) (velocityY $~! \v -> v - 0.05) >> return ()
    when (x > 0.95 ) (velocityX $~! \v -> v - 0.05) >> return ()
    when (x < -0.95) (velocityX $~! \v -> v + 0.05) >> return ()
    postRedisplay Nothing
    mapElementsCollision velocityY velocityX map pos
    where ball  = 50.0
          earth = 500.0

mapElementsCollision :: IORef GLfloat -> 
                        IORef GLfloat -> 
                        [((GLfloat, GLfloat, GLfloat),(GLfloat, GLfloat, GLfloat))] -> 
                        IORef (GLfloat, GLfloat) -> IO ()
mapElementsCollision velocityY velocityX map pos = do
    (x,y) <- get pos
    forM_ map $ \((a,b,c), (d,e,f)) -> do
        vY <- get velocityY
        let collsion  = x >= a - 0.05 && x <= d + 0.05 && (y - 0.05 >= b - 0.001 && y - 0.05 <= b + 0.001)
        when collsion (velocityY $~! (\v -> v * (ball - earth) / (ball + earth))) >> return ()
        when (collsion && y - 0.05 < b && vY < 0.003) (pos $~! (\(x',y') -> (x', b + 0.05))) >> return ()
        postRedisplay Nothing
        where ball      = 50.0
              earth     = 500.0