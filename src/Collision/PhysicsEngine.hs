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
        case getType game of
            PolygonType -> return ()
            BallType    -> do
                let (vX, vY) = getVelocity game
                    (x, y)   = getCenter game
                    v        = vY + acc * dt
                    newGame  = setVelocity (vX, v) game
                    newX     = vX * dt
                    newY     = v * dt + 0.5 * acc * dt ^ 2
                objects $~! (\b -> insert (getId game) (GameObject (setOffset (newX, newY) newGame)) b)
    where acc = -9.80665

earth :: Float -> Float
earth v = v * (game - earth) / (game + earth)
          where game  = 50.0
                earth = 500.0

collisionBoundaries :: IORef (Map Int GameObject) -> IO ()
collisionBoundaries objects = do
    b_ <- get objects
    forM_ b_ $ \(GameObject game) -> do
        case getType game of
            PolygonType -> return ()
            BallType    -> do
                            let (x, y)   = getCenter game
                                (vX, vY) = getVelocity game
                                id       = getId game
                                radius   = getRadius game
                                min      = (-1.0 + radius)
                                max      = (1.0 - radius)
                            when (y < min) $ objects $~! \b -> insert id (GameObject (setOffset (0, min - y) (setVelocity (vX, earth vY) game))) b
                            -- when (x > max) $ objects $~! \b -> insert id (GameObject (setOffset (max, y)  game)) b
                            -- when (x < min) $ objects $~! \b -> insert id (GameObject (setOffset (min, y) game)) b
                            -- when (x > max || x < min) $ objects $~! \b -> insert id (GameObject (setVelocity (earth vX, vY) game)) b