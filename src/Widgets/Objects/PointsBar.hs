module Widgets.Objects.PointsBar(
    PointsBar(..)
) where

import Graphics.UI.GLUT
import Text.Printf

import Widgets.WidgetObject
import Common.Drawable

data PointsBar = PointsBar {
    value :: Float
}

instance Widget_ PointsBar where
    setValue _value pointsBar = pointsBar { value = _value }
    getValue pointsBar = value pointsBar
    setCallback pointsBar = error "Not implemented exception"
    getCallback pointsBar = error "Not implemented exception"
    setOptions _ _ = error "Not implemented exception"
    getOptions _   = error "Not implemented exception"
    draw pointsBar = preservingMatrix $ do
                        getColor3f 1 0 1
                        translate $ Vector3 (0.7::GLfloat) (0.95::GLfloat) 0
                        rasterPos (Vertex2 (0.0::GLfloat) (0.0::GLfloat))
                        renderString Helvetica18 $ printf "Points %f" (value pointsBar)

instance Drawable_ PointsBar where
    render pointsBar = draw pointsBar