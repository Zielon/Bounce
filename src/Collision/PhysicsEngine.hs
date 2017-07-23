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

import Collision.AABB

import GameObjects.Objects.Ball
import GameObjects.GameObject

updateGravity :: (GameObject a) => IORef (Map Int a) -> Float -> IO ()
updateGravity balls dt = do
    b_ <- get balls
    forM_ b_ $ \ball -> do
        let (vX, vY) = getVelocity ball
            (x, y)   = getCenter ball
            id       = getId ball
            v        = vY + acc * dt
            ball_v = setVelocity (vX, v) ball
        balls $~! (\b -> insert id (setOffset ((x + vX * dt), ((y + v * dt) + 0.5 * acc * dt ^ 2)) ball_v) b) 
    where acc = -9.80665

earth :: Float -> Float
earth v = v * (ball - earth) / (ball + earth)
          where ball  = 50.0
                earth = 500.0

collisionBoundaries :: (GameObject a) => IORef (Map Int a) -> IO ()
collisionBoundaries balls = do
    b_ <- get balls
    forM_ b_ $ \ball -> do
        let (x, y)   = getCenter ball
            (vX, vY) = getVelocity ball
            id       = getId ball
        when (y < -0.95) $ balls $~! \b -> insert id (setVelocity (vX, earth vY) (setOffset (x, -0.95) ball)) b
        when (x > 0.95 ) $ balls $~! \b -> insert id (setOffset (0.95, y)  ball) b
        when (x < -0.95) $ balls $~! \b -> insert id (setOffset (-0.95, y) ball) b
        when (x > 0.95  || x < -0.95) $ balls $~! \b ->  insert id (setVelocity (earth vX, vY) ball) b