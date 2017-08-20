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
    setValue _value forceBar = forceBar { value = _value }
    getValue forceBar = value forceBar
    setCallback forceBar = error "Not implemented exception"
    getCallback forceBar = error "Not implemented exception"
    setOptions _ _ = error "Not implemented exception"
    getOptions _   = error "Not implemented exception"
    draw forceBar = preservingMatrix $ do
                        getColor3f 1 0 0
                        renderPrimitive Polygon $ mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) $ points
                        translate $ Vector3 (-0.95::GLfloat) (0.95::GLfloat) 0
                        getColor3f 1 0 1
                        rasterPos (Vertex2 (0.0::GLfloat) (0.0::GLfloat))
                        renderString Helvetica18 $ printf "%.1f%%" (value forceBar * 10)
          where scale = 2/10
                force = -1 + scale * value forceBar
                x  = -1.0
                y  = -0.95
                points = [(x, force), (x, x), (y, x), (y, force)]

instance Drawable_ ForceBar where
    render forceBar = draw forceBar