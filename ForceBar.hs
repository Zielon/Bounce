module ForceBar where

import FloorEngine

getBar :: Float -> Floor
getBar forceLevel = Floor tl tr bl br 1
    where scale = 2/10
          force = -1 + scale * forceLevel
          tl = (-1.0, force, 1.0)   -- | top left
          tr = (-0.95, force, 1.0)  -- | top right
          bl = (-1.0, -1.0, 1.0)    -- | bottom left
          br = (-0.95, -1.0, 1.0)   -- | bottom right