module GameObjects.ForceBar where

import Engines.FloorEngine
import Graphics.UI.GLUT

getBar :: Float -> Floor
getBar forceLevel = Floor x y tl tr bl br 1 (1::GLfloat, 0::GLfloat, 0::GLfloat) 1 1
    where scale = 2/10
          force = -1 + scale * forceLevel
          x  = -1.0
          y  = -0.95
          tl = (x, force, 1.0)   -- | top left
          tr = (y, force, 1.0)  -- | top right
          bl = (x, x, 1.0)    -- | bottom left
          br = (y, x, 1.0)   -- | bottom right