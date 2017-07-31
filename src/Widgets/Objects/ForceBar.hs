module Widgets.Objects.ForceBar(
    ForceBar(..)
) where

import Graphics.UI.GLUT
import Text.Printf

import Widgets.WidgetObject
import Common.Drawable

data ForceBar = ForceBar {
    value :: Float
}

instance Widget_ ForceBar where
    setValue _value forceBar = forceBar { value = v + _value } where v = value forceBar
    getValue forceBar = value forceBar
    draw forceBar = preservingMatrix $ do
                        getColor3f 1 0 1
                        translate $ Vector3 (-0.85::GLfloat) (0.95::GLfloat) 0
                        rasterPos (Vertex2 (0.0::GLfloat) (-0.025::GLfloat))
                        renderString Helvetica18 $ printf "Force %f" (value forceBar)

instance Drawable_ ForceBar where
    render forceBar = draw forceBar