module Widgets.Objects.PointsBar(
    PointsBar(..)
) where

import Graphics.UI.GLUT
import Widgets.WidgetObject
import Text.Printf

data PointsBar = PointsBar {
    value :: Int
}

instance Widget_ PointsBar where
    setValue _value pointsBar = pointsBar { value = _value }
    getValue pointsBar = value pointsBar
    draw pointsBar = preservingMatrix $ do
                        getColor3f 1 0 1
                        translate $ Vector3 (0.65::GLfloat) (0.95::GLfloat) 0
                        rasterPos (Vertex2 (0.0::GLfloat) (-0.025::GLfloat))
                        renderString Helvetica18 $ printf "Points %d" (value pointsBar)