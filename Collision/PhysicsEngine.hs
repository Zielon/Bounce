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

import GameObjects.Objects.Ball
import GameObjects.Positionable

updateGravity :: IORef Ball -> Float -> IO ()
updateGravity ball dt = do
    ball $~! \b -> setVelocity b $ \(vX,vY) -> (vX, (vY + acc * dt))
    ball' <- get ball
    let (vX, vY) = getVelocity ball'
    ball $~! \b -> setPosition b $ \(x,y) -> ((x + vX * dt),((y + vY * dt) + 0.5 * acc * dt ^ 2))
    where acc = -9.80665

earth :: Float -> Float
earth v = v * (ball - earth) / (ball + earth)
          where ball  = 50.0
                earth = 500.0

collisionBoundaries :: IORef Ball -> IO ()
collisionBoundaries ball = do
    ball' <- get ball
    let (x,y) = getPosition ball'
    when (y < -0.95) $ ball $~! \b -> setVelocity b $ \(vX,vY) -> (vX, earth vY)
    when (x > 0.95  || x < -0.95) $ ball $~! \b -> setVelocity b $ \(vX,vY) -> (earth vX, vY)
    -- Move back the ball when overstep the boundaries
    when (y < -0.95) $ ball $~! \b -> setPosition b $ \(x,y) -> (x, -0.95)
    when (x > 0.95 ) $ ball $~! \b -> setPosition b $ \(x,y) -> (0.95, y)
    when (x < -0.95) $ ball $~! \b -> setPosition b $ \(x,y) -> (-0.95, y)

-- | Collision for the ball with floors
--
collisionEdges :: IORef Ball -> IORef (Map Int Floor) -> IO ()
collisionEdges ball dictionary = do
    floors' <- get dictionary
    forM_ floors' $ \f -> do
        ball' <- get ball
        let (x,y) = getPosition ball'
            (min_x, min_y) = getMin f
            (max_x, max_y) = getMax f
            radius' = radius ball'
        case checkBallCollision ball' f of
            None  -> return ()
            Left  -> ball $~! (\b -> setVelocity b $ \(vX,vY) -> (earth vX, vY))
                                   >> ball $~! (\b -> setPosition b $ \(x,y) -> (min_x - radius', y))
            Right -> ball $~! (\b -> setVelocity b $ \(vX,vY) -> (earth vX, vY)) 
                                   >> ball $~! (\b -> setPosition b $ \(x,y) -> (max_x + radius', y)) 
            Under -> ball $~! (\b -> updateScore b (id f))
                                   >> ball $~! (\b -> setPosition b $ \(x,y) -> (x, min_y - radius'))
                                   >> ball $~! (\b -> setVelocity b $ \(vX,vY) -> (vX, earth vY))
                                   >> dictionary $~! (\d -> moveSingle f (-(abs (y + radius' - min_y))) d) -- actually move up
            Top  -> ball $~! (\b -> updateScore b (id f))
                                   >> ball $~! (\b -> setPosition b $ \(x,y) -> (x, max_y + radius'))
                                   >> ball $~! (\b -> setVelocity b $ \(vX,vY) -> (vX, earth vY))
                                   >> dictionary $~! (\d -> moveSingle f (abs (y - radius' - max_y)) d)