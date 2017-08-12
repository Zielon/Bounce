module API.Bindings (
  reshape,
  updateKeysBindings,
  keyboard,
  mouseMotion,
  moveObject) 
where

import Graphics.UI.GLUT
import Graphics.UI.GLUT.Callbacks
import Data.IORef
import Data.Fixed
import Data.Bool
import Text.Printf
import Data.Map
import Data.List           hiding (lookup, insert)
import Prelude             hiding (lookup)
import Control.Monad 

import Widgets.Widget
import GameObjects.Objects.Ball      as Ball
import GameObjects.Objects.Polygon   as Polygon
import Collision.Helpers

import API.Display
import API.Keys
import API.Buttons
import API.Ternary

reshape :: ReshapeCallback
reshape size = do 
  viewport $= (Position 0 0, size)

getPosition :: Position -> Vector
getPosition (Position x y) = (xw, -yw)
    where xw = (realToFrac x) / 400.0 - 1.0
          yw = (realToFrac y) / 400.0 - 1.0

moveObject :: IORef (Map Int GameObject) -> IORef Vector -> MotionCallback
moveObject arena mouse position = do
  objects <- get arena
  mouse ^& (\m -> (xw, yw))
  let value = find (\(k, (GameObject v)) -> getHovered v == True ) $ toList objects
  case value of
      Nothing                    -> return ()
      Just (k, (GameObject v)) -> do 
        arena ^& (\p -> insert k (GameObject (setOffset (xw-o_x, yw-o_y) v)) p)
        where (o_x, o_y) = getCenter v

  where (xw, yw) = getPosition position

mouseMotion :: IORef Vector -> MotionCallback
mouseMotion mouse position = do
  mouse ^& (\m -> (xw, yw))
  where (xw, yw) = getPosition position

updateKeysBindings :: IORef (Map GameKey Bool) -> IORef (Map Int GameObject) -> IORef (Map Int Widget) -> IO ()
updateKeysBindings refkeys arena widgets = do
  keys     <- get refkeys
  objects  <- get arena
  widgets' <- get widgets

  let (Just leftKey)  = lookup GameKeyLeft  keys
  let (Just rightKey) = lookup GameKeyRight keys
  let (Just forceKey) = lookup GameKeyForce keys

  case lookup 1 widgets' of         -- Get the force bar widget and update it
    Nothing         -> return ()
    Just (Widget w) -> do
        let force = getValue w
            value = force + 0.1 > maxForce ? maxForce :? force + 0.1
        forceKey == True ? widgets ^& (\m -> insert 1 (Widget $ setValue value w) m) :?
                           widgets ^& (\m -> insert 1 (Widget $ setValue 0 w) m)

  where maxForce = 10.0

keyboard :: IORef (Map GameKey Bool) -> IORef (Map Int GameObject) -> KeyboardMouseCallback
keyboard keys arena key state _ _ = do
  arena'    <- get arena
  keys'     <- get keys
  index     <- newIORef 1

  let value = find (\(k ,v) -> v == True) $ toList keys'

  -- TODO delete this part [Control the 4 objects with the consquent ids throughout keys (1,2,3,4)]
  case value of
    Nothing       -> return ()
    (Just (k, b)) -> case k of 
                        GameKeyOne   -> index ^& (\i -> 1)
                        GameKeyTwo   -> index ^& (\i -> 2)
                        GameKeyThree -> index ^& (\i -> 3)
                        GameKeyFour  -> index ^& (\i -> 4)
                        _            -> return ()
  i <- get index

  case lookup i arena' of
    Nothing             -> return ()
    Just (GameObject z) -> case key of
                              (SpecialKey KeyLeft)  -> arena ^& (\p -> insert (getId z) (GameObject (setVelocity ((-precision), 0) z)) p)
                              (SpecialKey KeyRight) -> arena ^& (\p -> insert (getId z) (GameObject (setVelocity (precision, 0) z)) p)
                              (SpecialKey KeyUp)    -> arena ^& (\p -> insert (getId z) (GameObject (setVelocity (0, precision) z)) p)
                              (SpecialKey KeyDown)  -> arena ^& (\p -> insert (getId z) (GameObject (setVelocity (0, (-precision)) z)) p)
                              (Char ' ')            -> state == Down ? (updateKey keys GameKeyForce True) :? (updateKey keys GameKeyForce False)
                              (Char '1')            -> state == Down ? (updateKey keys GameKeyOne True) :? (updateKey keys GameKeyOne False)
                              (Char '2')            -> state == Down ? (updateKey keys GameKeyTwo True) :? (updateKey keys GameKeyTwo False)
                              (Char '3')            -> state == Down ? (updateKey keys GameKeyThree True) :? (updateKey keys GameKeyThree False)
                              (Char '4')            -> state == Down ? (updateKey keys GameKeyFour True) :? (updateKey keys GameKeyFour False)
                              _                     -> return ()
  where precision = 0.005
