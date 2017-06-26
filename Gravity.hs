{-# LANGUAGE FlexibleContexts #-}

module Gravity where

import Graphics.UI.GLUT
import Control.Monad    
import Data.IORef
import Control.Concurrent

updateGravity :: IORef GLfloat -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> 
                 [((GLfloat, GLfloat, GLfloat),(GLfloat, GLfloat, GLfloat))] -> IO ()
updateGravity velocityX velocityY pos map = do
    vX <- get velocityX
    (velocityY $~! \v -> v + acc * dt)
    vY <- get velocityY
    pos $~! \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2))
    collision velocityY velocityX map pos
    where acc = -9.80665
          dt  = 0.0005

collision :: IORef GLfloat -> IORef GLfloat ->  
             [((GLfloat, GLfloat, GLfloat),(GLfloat, GLfloat, GLfloat))] ->  
             IORef (GLfloat, GLfloat) -> IO ()
collision velocityY velocityX map pos = do
    (x,y) <- get pos
    -- The map boundaries
    when (y < -0.90) (velocityY $~! \v -> v * (ball - earth) / (ball + earth)) >> return ()
    when (y > 0.95 ) (velocityY $~! \v -> v - 0.05) >> return ()
    when (x > 0.95 ) (velocityX $~! \v -> v - 0.05) >> return ()
    when (x < -0.95) (velocityX $~! \v -> v + 0.05) >> return ()
    mapElementsCollision velocityY velocityX map pos
    where ball  = 50.0
          earth = 500.0

mapElementsCollision :: IORef GLfloat -> IORef GLfloat -> 
                        [((GLfloat, GLfloat, GLfloat),(GLfloat, GLfloat, GLfloat))] -> 
                        IORef (GLfloat, GLfloat) -> IO ()
mapElementsCollision velocityY velocityX map pos = do
    (x,y) <- get pos
    forM_ map $ \((a,b,c), (d,e,f)) -> 
        if x >= a - 0.05 && x <= d + 0.05 && y + 0.05 >= b then velocityY $~! \v -> v - 0.05
        --else if x >= a - 0.05 && x <= d + 0.05 && y - 0.05 < b then velocityY $~! \v -> v * (ball - earth) / (ball + earth)
        else return ()
        where ball  = 50.0
              earth = 500.0


