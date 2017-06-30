module Bindings (idle, display, reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Data.IORef
import Data.Fixed

import Display
import Data.Bool
import Keys

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

keyboardMouse :: IORef GLfloat ->
                 IORef GLfloat ->
                 IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse force velocityX velocityY p key state _ _ = 
  case key of
      (SpecialKey KeyLeft) ->
        if state == Down then velocityX $~! (\x -> x - 0.3)
        else return ()
      (SpecialKey KeyRight) ->
        if state == Down then velocityX $~! (\x -> x + 0.3)
        else return ()
      (Char ' ') ->
        if state == Down then force $~! \f -> (f + 1.0)
        else do
          force' <- get force
          let f = if force' > maxForce then maxForce else force'
          velocityY $~! (\vY-> if vY <= 0 then vY - f else vY + f) >> force $~! (\f -> 0.0)
      _ -> return ()
  where maxForce = 8.0