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
import Prelude hiding (lookup)
import Control.Monad 

import GameObjects.Ball
import GameObjects.Polygon

import API.Display
import API.Keys
import API.Ternary

reshape :: ReshapeCallback
reshape size = do viewport $= (Position 0 0, size)

updateKeysBindings :: IORef (Map GameKey Bool) -> IORef GLfloat -> IORef Ball -> IO ()
updateKeysBindings refkeys force ball = do
  keys   <- get refkeys
  gForce <- get force

  let (Just leftKey)  = lookup GameKeyLeft  keys
  let (Just rightKey) = lookup GameKeyRight keys
  let (Just forceKey) = lookup GameKeyForce keys

  when(leftKey)      $ ball  $~! \b -> setVelocity b (\(x,y) -> (x - 0.05, y))
  when(rightKey)     $ ball  $~! \b -> setVelocity b (\(x,y) -> (x + 0.05, y))
  when(forceKey)     $ force $~! \f -> f + 0.1 > maxForce ? maxForce :? f + 0.1
  when(not forceKey) $ ball  $~! (\b -> setVelocity b (\(vX,vY) -> vY <= 0 ? (vX, vY - gForce) :? (vX, vY + gForce))) >> force $~! (\f -> 0.0)

  where maxForce = 10.0

keyboardMouse :: IORef (Map GameKey Bool) -> IORef (Map Int GamePolygon) -> KeyboardMouseCallback
keyboardMouse keys polygons key state _ _ = do
  polygons' <- get polygons
  let (Just z) = lookup 1 polygons'

  case key of
      (SpecialKey KeyLeft)  -> polygons $~! (\p -> insert 1 (offset (-0.01, 0) z) p) --  state == Down ? (updateKey keys GameKeyLeft  True) :? (updateKey keys GameKeyLeft  False)
      (SpecialKey KeyRight) -> polygons $~! (\p -> insert 1 (offset (0.01, 0) z) p) -- state == Down ? (updateKey keys GameKeyRight True) :? (updateKey keys GameKeyRight False)
      (Char ' ')            -> state == Down ? (updateKey keys GameKeyForce True) :? (updateKey keys GameKeyForce False)
      (SpecialKey KeyUp)    -> polygons $~! (\p -> insert 1 (offset (0, 0.01) z) p)
      (SpecialKey KeyDown)  -> polygons $~! (\p -> insert 1 (offset (0, -0.01) z) p)
      _                     -> return ()