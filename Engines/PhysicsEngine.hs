{-# LANGUAGE FlexibleContexts #-}

module Engines.PhysicsEngine (
    updateGravity, 
    collisionBoundaries, 
    collisionEdges) 
where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (id, floor)
import Control.Monad    
import Data.IORef
import Control.Concurrent
import Text.Printf

import Engines.FloorEngine
import API.CollisionTests
import GameObjects.Ball

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
    --when (y > 0.95 ) $ pos $~! (\(x',y') -> (x', 0.95))
    when (x > 0.95 ) $ ball $~! \b -> setPosition b $ \(x,y) -> (0.95, y)
    when (x < -0.95) $ ball $~! \b -> setPosition b $ \(x,y) -> (-0.95, y)

-- Collisin for the ball with floors
collisionEdges :: IORef Ball -> IORef [Floor] -> IO ()
collisionEdges ball floors = do
    fls <- get floors
    forM_ fls $ \f -> do
        ball' <- get ball
        let (x,y) = getPosition ball'
        let (min_x, min_y, _) = bottom_left f
        let (max_x, max_y, _) = top_right f
        case ballTestAABB ball' f of
            None       -> return ()
            AxisX      -> ball $~! (\b -> setVelocity b $ \(vX,vY) -> (earth vX, vY))
            UnderAxisY -> ball $~! (\b -> updateScore b (id f)) 
                                   >> ball $~! (\b -> setVelocity b $ \(vX,vY) -> (vX, earth vY)) 
                                   >> floors $~! (\floor' -> moveDownSingle f (-(abs (y + radius - min_y))) floor')
            OverAxisY  -> ball $~! (\b -> updateScore b (id f)) 
                                   >> ball $~! (\b -> setVelocity b $ \(vX,vY) -> (vX, earth vY)) 
                                   >> floors $~! (\floor' -> moveDownSingle f (abs (y - radius - max_y)) floor')
        where radius = 0.05
              edge   = 0.025