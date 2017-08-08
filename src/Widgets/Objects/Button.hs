module Widgets.Objects.Button where

import Graphics.UI.GLUT
import Text.Printf

data Button = Button {
    value :: Float
}

instance Widget_ ForceBar where
    setValue _value button = button { value = _value }
    getValue button = value button
    setCallback button = error "Not implemented exception"
    getCallback button = error "Not implemented exception"
    draw button = preservingMatrix $ do
                    getColor3f 1 0 1
                    rasterPos (Vertex2 (0.0::GLfloat) (0.0::GLfloat))
                    renderString Helvetica18 $ printf "%.1f%%" (value forceBar * 10)