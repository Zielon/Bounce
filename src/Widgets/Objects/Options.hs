module Widgets.Objects.Options where

import Graphics.UI.GLUT
import Text.Printf
import Control.Monad
import Collision.Helpers

import Data.IORef
import Data.Map  as M
import Data.List as L

import API.Ternary
import Widgets.WidgetObject
import Common.Drawable

data Options = Options {
    value    :: Float,
    settings :: Map Setting Bool
}

instance Widget_ Options where
    setValue _value options = options { value = _value }
    getValue options = value options
    setCallback options = error "Not implemented exception"
    getCallback options = error "Not implemented exception"
    setOptions options (k, v) = options { settings = M.insert k v (settings options) }
    getOptions options = settings options
    draw options = do 
        i <- newIORef (0.85::GLfloat)
        forM_ (M.toList (settings options)) $ \(k, v) ->
            preservingMatrix $ do
            getColor3f 1 0 0
            i' <- get i
            i ^& \e -> e - 0.05
            translate $ Vector3 (-0.95::GLfloat) i' 0
            rasterPos (Vertex2 (0.0::GLfloat) (0.0::GLfloat))
            renderString Helvetica18 $ printf "%-8s -> %s" (show k) (v == True ? "ON" :? "OFF")

instance Drawable_ Options where
    render options = draw options