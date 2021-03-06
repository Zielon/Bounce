module API.Bindings (
  reshape,
  updateKeysBindings,
  keyboardMouse,
  mouseMotion,
  moveObject) 
where

import Graphics.UI.GLUT
import Graphics.UI.GLUT.Callbacks
import Data.IORef
import Data.Fixed
import Data.Bool
import Control.Concurrent
import Text.Printf
import Data.Map
import Data.List           hiding (lookup, insert)
import Prelude             hiding (lookup)
import Control.Monad 

import Widgets.Widget
import Widgets.Settings
import GameObjects.Objects.Ball      as Ball
import GameObjects.Objects.Polygon   as Polygon
import GameObjects.Objects.Segment   as S
import Factory.Producer
import Collision.Helpers
import Collision.VectorOperations
import Collision.RayReflection

import API.Display
import API.Keys
import API.Buttons
import API.Ternary

reshape :: IORef Size -> ReshapeCallback
reshape ref size = do
  ref ^& (\r -> size)
  viewport $= (Position 0 0, size)

getPosition :: Position -> Size -> Vector
getPosition (Position xP yP) (Size xS yS) = (xw, -yw)
    where xw = (realToFrac xP) / ((realToFrac xS) / 2) - 1.0
          yw = (realToFrac yP) / ((realToFrac yS) / 2) - 1.0

moveObject :: IORef (Map Int GameObject) -> IORef Vector -> IORef Size -> MotionCallback
moveObject arena mouse size position = do
  objects     <- get arena
  windows     <- get size
  oldPosition <- get mouse

  let newPosition = getPosition position windows
  mouse ^& (\m -> newPosition)
  let value = find (\(k, (GameObject v)) -> getHovered v == True ) $ toList objects
  case value of
      Nothing                  -> return ()
      Just (k, (GameObject v)) -> do 
        arena ^& (\p -> insert k (GameObject (setOffset diff v)) p)
        where diff = newPosition -. oldPosition

mouseMotion :: IORef Vector -> IORef Size -> MotionCallback
mouseMotion mouse size position = do
  windows <- get size
  let (xw, yw) = getPosition position windows
  mouse ^& (\m -> (xw, yw))

updateKeysBindings :: IORef (Map GameKey Bool) -> IORef (Map Int GameObject) -> IORef (Map Int Widget) -> IO ()
updateKeysBindings refkeys arena widgets = do
  keys     <- get refkeys
  objects  <- get arena
  widgets' <- get widgets

  let (Just forceKey) = lookup GameKeyForce keys
      (Just oneKey)   = lookup GameKeyOne   keys
      (Just twoKey)   = lookup GameKeyTwo   keys
      (Just threeKey) = lookup GameKeyThree keys

  -- Settings
  case lookup 3 widgets' of
    Nothing         -> return ()
    Just (Widget w) -> do
      let cast       = setOptions w (Cast, oneKey)
      let reflection = setOptions cast (Reflection, twoKey)
      widgets ^& (\m -> insert 3 (Widget $ reflection) m)

  -- Force bar
  case lookup 1 widgets' of         -- Get the force bar widget and update it
    Nothing         -> return ()
    Just (Widget w) -> do
        let force = getValue w
            value = force + 0.1 > maxForce ? maxForce :? force + 0.1
        forceKey == True ? widgets ^& (\m -> insert 1 (Widget $ setValue value w) m) :?
                           widgets ^& (\m -> insert 1 (Widget $ setValue 0 w) m)

  where maxForce = 10.0

keyboardMouse :: IORef (Map GameKey Bool) -> IORef (Map Int GameObject) -> IORef (Map Int Widget) -> IORef Size -> IORef [Segment] -> KeyboardMouseCallback
keyboardMouse keys arena widgets windows reflections key state _ position = do
  arena'    <- get arena
  keys'     <- get keys
  widgets'  <- get widgets
  windows'  <- get windows

  let pos = getPosition position windows'

  case lookup 3 widgets' of
    Nothing         -> return ()
    Just (Widget w) -> do
      let (Just reflect) = lookup Reflection $ getOptions w
      reflect == True && state == Down ? reflections ^& (\_ -> getSegments 5 0.5 pos) :? return ()

  case key of
    (Char ' ') -> state == Down ? (updateKey keys GameKeyForce True) :? (updateKey keys GameKeyForce False)
    (Char '1') -> state == Down ? keys ^& (\k -> let (Just v) = lookup GameKeyOne k   in insert GameKeyOne (not v) k)   :? return ()
    (Char '2') -> state == Down ? keys ^& (\k -> let (Just v) = lookup GameKeyTwo k   in insert GameKeyTwo (not v) k)   :? return ()
    (Char '3') -> state == Down ? keys ^& (\k -> let (Just v) = lookup GameKeyThree k in insert GameKeyThree (not v) k) :? return ()
    (Char '4') -> state == Down ? keys ^& (\k -> let (Just v) = lookup GameKeyFour k  in insert GameKeyFour (not v) k)  :? return ()
    _          -> return ()
