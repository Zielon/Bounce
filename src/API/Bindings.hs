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

import GameObjects.Objects.Ball      as Ball
import GameObjects.Objects.Polygon   as Polygon
import GameObjects.Objects.BaseClass as Base

import API.Display
import API.Keys
import API.Ternary

reshape :: ReshapeCallback
reshape size = do viewport $= (Position 0 0, size)

updateKeysBindings :: IORef (Map GameKey Bool) -> IORef GLfloat -> IORef Ball -> IO ()
updateKeysBindings refkeys force ball = do
  keys   <- get refkeys
  gForce <- get force
  ball'  <- get ball

  let (Just leftKey)  = lookup GameKeyLeft  keys
  let (Just rightKey) = lookup GameKeyRight keys
  let (Just forceKey) = lookup GameKeyForce keys
  let (x,y)    = getCenter ball' 
  let (vX, vY) = getVelocity ball'

  when(leftKey)      $ ball  $~! \b -> Base.setVelocity (x - 0.05, y) b
  when(rightKey)     $ ball  $~! \b -> Base.setVelocity (x + 0.05, y) b
  when(forceKey)     $ force $~! \f -> f + 0.1 > maxForce ? maxForce :? f + 0.1
  when(not forceKey) $ ball  $~! (\b -> Base.setVelocity (vY <= 0 ? (vX, vY - gForce) :? (vX, vY + gForce)) b) >> force $~! (\f -> 0.0)

  where maxForce = 10.0

keyboardMouse :: IORef (Map GameKey Bool) -> IORef (Map Int GamePolygon) -> KeyboardMouseCallback
keyboardMouse keys polygons key state _ _ = do
  polygons' <- get polygons
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

  let (Just z) = lookup i polygons'

  case key of
      (SpecialKey KeyLeft)  -> polygons $~! (\p -> insert (Polygon.id z) (Base.setVelocity (-0.02, 0) z) p)
      (SpecialKey KeyRight) -> polygons $~! (\p -> insert (Polygon.id z) (Base.setVelocity (0.02, 0) z) p)
      (SpecialKey KeyUp)    -> polygons $~! (\p -> insert (Polygon.id z) (Base.setVelocity (0, 0.02) z) p)
      (SpecialKey KeyDown)  -> polygons $~! (\p -> insert (Polygon.id z) (Base.setVelocity (0, -0.02) z) p)
      (Char ' ')            -> state == Down ? (updateKey keys GameKeyForce True) :? (updateKey keys GameKeyForce False)
      (Char '1')            -> state == Down ? (updateKey keys GameKeyOne True) :? (updateKey keys GameKeyOne False)
      (Char '2')            -> state == Down ? (updateKey keys GameKeyTwo True) :? (updateKey keys GameKeyTwo False)
      (Char '3')            -> state == Down ? (updateKey keys GameKeyThree True) :? (updateKey keys GameKeyThree False)
      (Char '4')            -> state == Down ? (updateKey keys GameKeyFour True) :? (updateKey keys GameKeyFour False)
      _                     -> return ()