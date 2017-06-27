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
        if state == Down then velocityX $~! (\x -> x - 0.15)
        else return ()
      (SpecialKey KeyRight) ->
        if state == Down then velocityX $~! (\x -> x + 0.15)
        else return ()
      (Char ' ') ->
        if state == Down then force $~! \f -> (f + 0.15)
        else do
          f <- get force
          velocityY $~! (\y-> y + f) >> force $~! (\f -> 0) >> p $~! \(x,y) -> (x, y + 0.01)
      _ -> return ()