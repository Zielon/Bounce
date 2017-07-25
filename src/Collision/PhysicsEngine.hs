{-# LANGUAGE FlexibleContexts #-}

module Collision.PhysicsEngine (
    updateGravity, 
    collisionBoundaries) 
where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (id, floor, Left, Right)
import Control.Monad    
import Data.IORef
import Control.Concurrent
import Text.Printf
import Data.Map

import GameObjects.Objects.Ball
import GameObjects.GameObject

updateGravity :: IORef (Map Int GameObject) -> Float -> IO ()
updateGravity objects dt = do
    b_ <- get objects
    forM_ b_ $ \(GameObject game) -> do
        let (vX, vY) = getVelocity game
            (x, y)   = getCenter game
            id       = getId game
            v        = vY + acc * dt
            game_v   = setVelocity (vX, v) game
            newVx    = x + vX * dt
            newVy    = (y + v * dt) + 0.5 * acc * dt ^ 2
        objects $~! (\b -> insert id (GameObject (setOffset (newVx, newVy) game_v)) b)
    where acc = -9.80665

earth :: Float -> Float
earth v = v * (game - earth) / (game + earth)
          where game  = 50.0
                earth = 500.0

collisionBoundaries :: IORef (Map Int GameObject) -> IO ()
collisionBoundaries objects = do
    b_ <- get objects
    forM_ b_ $ \(GameObject game) -> do
        let (x, y)   = getCenter game
            (vX, vY) = getVelocity game
            id       = getId game
        when (y < -0.95) $ objects $~! \b -> insert id (GameObject (setVelocity (vX, earth vY) (setOffset (x, -0.95) game))) b
        when (x > 0.95 ) $ objects $~! \b -> insert id (GameObject (setOffset (0.95, y)  game)) b
        when (x < -0.95) $ objects $~! \b -> insert id (GameObject (setOffset (-0.95, y) game)) b
        when (x > 0.95  || x < -0.95) $ objects $~! \b -> insert id (GameObject (setVelocity (earth vX, vY) game)) b