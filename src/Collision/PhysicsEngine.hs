{-# LANGUAGE FlexibleContexts #-}

module Collision.PhysicsEngine (
    updateGravity, 
    collisionBoundaries, 
    collisionEdges) 
where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (id, floor, Left, Right)
import Control.Monad    
import Data.IORef
import Control.Concurrent
import Text.Printf
import Data.Map

import Collision.AABB

import GameArea.FloorEngine

import GameObjects.Objects.Floor as Floor
import GameObjects.Objects.Ball
import GameObjects.Objects.BaseClass
import GameObjects.Positionable

updateGravity :: IORef Ball -> Float -> IO ()
updateGravity b dt = do
    ball <- get b
    let (vX, vY) = getVelocity ball
    let (x, y)   = getCenter ball
    b $~! \b -> setVelocity (vX, (vY + acc * dt)) b
    b $~! \b -> setOffset ((x + vX * dt), ((y + vY * dt) + 0.5 * acc * dt ^ 2)) b
    where acc = -9.80665

earth :: Float -> Float
earth v = v * (ball - earth) / (ball + earth)
          where ball  = 50.0
                earth = 500.0

collisionBoundaries :: IORef Ball -> IO ()
collisionBoundaries ball = do
    ball' <- get ball
    let (x,y) = getCenter ball'
    let (vX, vY) = getVelocity ball'
    when (y < -0.95) $ ball $~! \b -> setVelocity (vX, earth vY) b
    when (x > 0.95  || x < -0.95) $ ball $~! \b -> setVelocity (earth vX, vY) b
    -- Move back the ball when overstep the boundaries
    when (y < -0.95) $ ball $~! \b -> setOffset (x, -0.95) b
    when (x > 0.95 ) $ ball $~! \b -> setOffset (0.95,  y) b
    when (x < -0.95) $ ball $~! \b -> setOffset (-0.95, y) b

-- | Collision for the ball with floors
--
collisionEdges :: IORef Ball -> IORef (Map Int Floor) -> IO ()
collisionEdges ball dictionary = do
    floors' <- get dictionary
    forM_ floors' $ \f -> do
        ball' <- get ball
        let (x,y) = getCenter ball'
            (min_x, min_y) = getMin f
            (max_x, max_y) = getMax f
            radius' = radius ball'
            (vX, vY) = getVelocity ball'
        case checkBallCollision ball' f of
            None  -> return ()
            Left  -> ball $~! (\b -> setVelocity (earth vX, vY) b)
                                   >> ball $~! (\b -> setOffset (min_x - radius', y) b)
            Right -> ball $~! (\b -> setVelocity (earth vX, vY) b) 
                                   >> ball $~! (\b -> setOffset (max_x + radius', y) b) 
            Under -> ball $~! (\b -> updateScore b (Floor.id f))
                                   >> ball $~! (\b -> setOffset (x, min_y - radius') b)
                                   >> ball $~! (\b -> setVelocity (vX, earth vY) b)
                                   >> dictionary $~! (\d -> moveSingle f (-(abs (y + radius' - min_y))) d) -- actually move up
            Top  -> ball $~! (\b -> updateScore b (Floor.id f))
                                   >> ball $~! (\b -> setOffset (x, max_y + radius') b)
                                   >> ball $~! (\b -> setVelocity (vX, earth vY) b)
                                   >> dictionary $~! (\d -> moveSingle f (abs (y - radius' - max_y)) d)