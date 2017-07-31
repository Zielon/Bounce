module API.Bindings (
  reshape,
  updateKeysBindings,
  keyboardMouse) 
where

import Graphics.UI.GLUT
import Data.IORef
import Data.Fixed
import Data.Bool
import Data.Map
import Data.List           hiding (lookup, insert)
import Prelude             hiding (lookup)
import Control.Monad 

import Widgets.Widget
import GameObjects.Objects.Ball      as Ball
import GameObjects.Objects.Polygon   as Polygon

import API.Display
import API.Keys
import API.Ternary

reshape :: ReshapeCallback
reshape size = do viewport $= (Position 0 0, size)

updateKeysBindings :: IORef (Map GameKey Bool) -> IORef GLfloat -> IORef (Map Int GameObject) -> IORef (Map Int Widget) -> IO ()
updateKeysBindings refkeys force balls widgets = do
  keys     <- get refkeys
  gForce   <- get force
  balls'   <- get balls
  widgets' <- get widgets

  let (Just leftKey)  = lookup GameKeyLeft  keys
  let (Just rightKey) = lookup GameKeyRight keys
  let (Just forceKey) = lookup GameKeyForce keys

  case lookup 1 widgets' of
    Nothing         -> return ()
    Just (Widget w) -> when(forceKey) $ widgets $~! \m -> insert 1 (Widget (setValue (gForce + 0.1 > maxForce ? maxForce :? gForce + 0.1) w)) m

  return ()
  -- let (x,y)    = getCenter ball' 
  -- let (vX, vY) = getVelocity ball'

  -- when(leftKey)      $ ball  $~! \b -> setVelocity (x - 0.05, y) b
  -- when(rightKey)     $ ball  $~! \b -> setVelocity (x + 0.05, y) b
  -- when(not forceKey) $ ball  $~! (\b -> setVelocity (vY <= 0 ? (vX, vY - gForce) :? (vX, vY + gForce)) b) >> force $~! (\f -> 0.0)

  where maxForce = 10.0

keyboardMouse :: IORef (Map GameKey Bool) -> IORef (Map Int GameObject) -> KeyboardMouseCallback
keyboardMouse keys arena key state _ _ = do
  arena'    <- get arena
  keys'     <- get keys
  index     <- newIORef 1

  let value = find (\(k ,v) -> v == True) $ toList keys'

  -- TODO delete this part
  case value of
    Nothing       -> return ()
    (Just (k, b)) -> case k of 
                        GameKeyOne   -> index $~! (\i -> 1)
                        GameKeyTwo   -> index $~! (\i -> 2)
                        GameKeyThree -> index $~! (\i -> 3)
                        GameKeyFour  -> index $~! (\i -> 4)
                        _            -> return ()
  i <- get index

  case lookup i arena' of
    Nothing             -> return ()
    Just (GameObject z) -> case key of
                              (SpecialKey KeyLeft)  -> arena $~! (\p -> insert (getId z) (GameObject (setVelocity (-0.01, 0) z)) p)
                              (SpecialKey KeyRight) -> arena $~! (\p -> insert (getId z) (GameObject (setVelocity (0.01, 0) z)) p)
                              (SpecialKey KeyUp)    -> arena $~! (\p -> insert (getId z) (GameObject (setVelocity (0, 0.01) z)) p)
                              (SpecialKey KeyDown)  -> arena $~! (\p -> insert (getId z) (GameObject (setVelocity (0, -0.01) z)) p)
                              (Char ' ')            -> state == Down ? (updateKey keys GameKeyForce True) :? (updateKey keys GameKeyForce False)
                              (Char '1')            -> state == Down ? (updateKey keys GameKeyOne True) :? (updateKey keys GameKeyOne False)
                              (Char '2')            -> state == Down ? (updateKey keys GameKeyTwo True) :? (updateKey keys GameKeyTwo False)
                              (Char '3')            -> state == Down ? (updateKey keys GameKeyThree True) :? (updateKey keys GameKeyThree False)
                              (Char '4')            -> state == Down ? (updateKey keys GameKeyFour True) :? (updateKey keys GameKeyFour False)
                              _                     -> return ()