module Widgets.Objects.ForceBar(
    ForceBar(..)
) where

import Graphics.UI.GLUT

import Widgets.WidgetObject
import Common.Drawable

data ForceBar = ForceBar {
    value :: Int
}

instance Widget_ ForceBar where
    setValue _value forceBar = forceBar { value = _value }
    getValue forceBar = value forceBar
    draw forceBar = do return ()

instance Drawable_ ForceBar where
    render forceBar = draw forceBar