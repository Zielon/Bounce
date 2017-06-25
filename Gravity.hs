module Gravity where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Control.Concurrent

updateGravity :: IORef GLfloat -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> IO ()
updateGravity velocityX velocityY pos = do
    vX <- get velocityX
    (velocityY $~! \v -> v + acc * dt)
    vY <- get velocityY
    pos $~! \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2))
    collisionY velocityY pos >> collisionX velocityX pos
    where acc = -9.80665
          dt  = 0.0005

collisionX :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IO ()
collisionX velocityX pos = do
    (x,y) <- get pos
    if x > 0.95 then
        velocityX $~! \v -> v - 0.05
    else if x < -0.95 then
        velocityX $~! \v -> v + 0.05
    else return ()

collisionY :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IO ()
collisionY velocityY pos = do
    (x,y) <- get pos
    if y < -0.90 then
        velocityY $~! \y -> y * (ball - earth) / (ball + earth)
    else if y > 0.95 then
        velocityY $~! \y -> y - 0.05 
    else return ()
    where ball  = 50.0
          earth = 500.0