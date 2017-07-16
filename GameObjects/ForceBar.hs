module GameObjects.ForceBar where

import Engines.FloorEngine
import Graphics.UI.GLUT

getBar :: Float -> Floor
getBar forceLevel = Floor x y tl tr bl br 1 (1::GLfloat, 0::GLfloat, 0::GLfloat) 1 1
    where scale = 2/10
          force = -1 + scale * forceLevel
          x  = -1.0
          y  = -0.95
          tl = (x, force)   -- | top left
          tr = (y, force)  -- | top right
          bl = (x, x)    -- | bottom left
          br = (y, x)   -- | bottom right