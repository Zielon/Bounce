module Bindings (idle, display, reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Data.IORef
import Display
import Data.Bool
import Keys

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

updateLeft :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IO ()
updateLeft delta p = 
  (p $~! \(x,y) -> if x - 0.05 > -1 then (x-0.05,y) else (x,y)) >> delta $~! (\x -> x + 0.2)

updateRight :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IO ()
updateRight delta p = 
  (p $~! \(x,y) -> if x + 0.05 < 1  then (x+0.05,y) else (x,y)) >> delta $~! (\x -> x - 0.2)

updateUp :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IO ()
updateUp delta p = 
  p $~! \(x,y) -> if y + 0.1 < 1 then (x,y+0.1) else (x,y)

updateDown :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> IO ()
updateDown delta p = 
  p $~! \(x,y) -> if y - 0.1 > -1 then (x,y-0.1) else (x,y)

left :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> [IORef (SpecialKey, Bool)] -> IO ()
left delta p keys = do
  up   <- getStatus keys KeyUp
  down <- getStatus keys KeyDown
  --updateLeft delta p
  -- if up == True then updateUp delta p
  -- else if down == True then updateDown delta p
  return ()

right :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> [IORef (SpecialKey, Bool)] -> IO ()
right delta p keys = do
  up   <- getStatus keys KeyUp
  down <- getStatus keys KeyDown
  --updateRight delta p
  -- if up == True then updateUp delta p
  -- else if down == True then updateDown delta p
  return ()   

up :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> [IORef (SpecialKey, Bool)] -> IO ()
up delta p keys = do
  left  <- getStatus keys KeyLeft
  right <- getStatus keys KeyRight
  --updateUp delta p
  --if left == True then updateLeft delta p
  --else if right == True then updateRight delta p
  return ()

-- 0 left
-- 1 right
-- 2 up
-- 3 down
keyboardMouse :: IORef GLfloat ->
                 IORef GLfloat ->
                 IORef GLfloat -> 
                 [IORef (SpecialKey, Bool)] -> 
                 IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse force velocityX velocityY keys delta p key state _ _ = case key of

      (SpecialKey KeyLeft) ->
        if state == Down then 
          update keys 0 True >> left delta p keys >> velocityX $~! (\x -> x - 0.1)
        else update keys 0 False >> delta $~! (*0)

      (SpecialKey KeyRight) ->
        if state == Down then
          update keys 1 True >> right delta p keys >> velocityX $~! (\x -> x + 0.1)
        else
          update keys 1 False >> delta $~! (*0)

      (Char ' ') ->
        if state == Down then 
          update keys 2 True >> up delta p keys >> force $~! \f -> f + 0.1
        else do
          f <- get force
          update keys 2 False >> delta $~! (*0) >> velocityY $~! (\y-> y + f) >> (force $~! \f -> 0.0) >> p $~! \(x,y) -> (x, y + 0.01)          
          
      _ -> return ()
