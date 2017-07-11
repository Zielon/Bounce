module API.Bindings (
  reshape, 
  keyboardMouse) 
where

import Graphics.UI.GLUT
import Data.IORef
import Data.Fixed
import Data.Bool
import Data.Map
import Prelude hiding (lookup)
import Control.Monad 

import GameObjects.Ball
import API.Display
import API.Keys
import API.Helpers

reshape :: ReshapeCallback
reshape size = do viewport $= (Position 0 0, size)

updateKeysBindings :: IORef (Map GameKey Bool) -> IORef GLfloat -> IORef Ball -> IO ()
updateKeysBindings refkeys force ball = do
  keys   <- get refkeys
  gForce <- get force

  let (Just leftKey)  = lookup GameKeyLeft  keys
  let (Just rightKey) = lookup GameKeyRight keys
  let (Just forceKey) = lookup GameKeyForce keys

  when(leftKey)      $ ball  $~! \b -> setVelocity b (\(x,y) -> (x - 0.25, y))
  when(rightKey)     $ ball  $~! \b -> setVelocity b (\(x,y) -> (x + 0.25, y))
  when(forceKey)     $ force $~! \f -> f + 0.25 > maxForce ? maxForce :? f + 0.25
  when(not forceKey) $ ball  $~! (\b -> setVelocity b (\(vX,vY) -> vY <= 0 ? (vX, vY - gForce) :? (vX, vY + gForce))) >> force $~! (\f -> 0.0)

  where maxForce = 10.0

keyboardMouse :: IORef GLfloat -> IORef Ball -> IORef (Map GameKey Bool) -> KeyboardMouseCallback
keyboardMouse force ball keys key state _ _ = do
  case key of
      (SpecialKey KeyLeft)  -> state == Down ? (updateKey keys GameKeyLeft  True) :? (updateKey keys GameKeyLeft  False)
      (SpecialKey KeyRight) -> state == Down ? (updateKey keys GameKeyRight True) :? (updateKey keys GameKeyRight False)
      (Char ' ')            -> state == Down ? (updateKey keys GameKeyForce True) :? (updateKey keys GameKeyForce False)
      _                     -> return ()

  updateKeysBindings keys force ball