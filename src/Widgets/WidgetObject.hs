{-# LANGUAGE ExistentialQuantification #-}

module Widgets.WidgetObject(
    Widget(..),
    Widget_(..),
    Setting(..),
    getColor3f
) where

import Graphics.UI.GLUT
import Widgets.Settings
import Data.Map

class Widget_ a where
    draw        :: a -> IO ()
    getOptions  :: a -> Map Setting Bool
    setOptions  :: a -> (Setting, Bool) -> a
    getValue    :: a -> Float
    setValue    :: Float -> a -> a
    setCallback :: (a -> IO ()) -> a -> a  -- A callback after a click
    getCallback :: a -> (a -> IO ())

data Widget = forall a. Widget_ a => Widget a

getColor3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
getColor3f x y z = color $ Color3 ((x+1)/2) ((y+1)/2) (((z+1)/2) :: GLfloat)
