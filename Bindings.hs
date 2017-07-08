module Bindings (
  idle, 
  display, 
  reshape, 
  keyboardMouse) 
where
 
import Graphics.UI.GLUT
import Data.IORef
import Data.Fixed

import Ball
import Display
import Data.Bool
import Keys

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

keyboardMouse :: IORef GLfloat -> IORef Ball -> KeyboardMouseCallback
keyboardMouse force ball key state _ _ = do
  case key of
      (SpecialKey KeyLeft) ->
        if state == Down then (putStrLn "<- | Left") >> ball $~! \b -> setVelocity b (\(x,y) -> (x - 0.25, y))
        else return ()
      (SpecialKey KeyRight) ->
        if state == Down then (putStrLn "-> | Right") >> ball $~! \b -> setVelocity b (\(x,y) -> (x + 0.25, y))
        else return ()
      (Char ' ') ->
        if state == Down then force $~! \f -> if f + 0.25 > maxForce then maxForce else f + 0.25
        else do
          f <- get force
          putStrLn "Space"
          ball $~! (\b -> setVelocity b (\(vX,vY) -> if vY <= 0 then (vX, vY - f) else (vX, vY + f))) >> force $~! (\f -> 0.0)
      _ -> return ()
  where maxForce = 10.0