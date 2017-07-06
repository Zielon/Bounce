module ForceBar where

import FloorGenerator

getBar :: Float -> Floor
getBar forceLevel = Floor tl tr bl br
    where scale = 2/10
          force = scale * forceLevel
          value = if force == 0 then -1 else if forceLevel <= 5 then -1+force else force
          tl = (-1.0, value, 1.0)   -- | top left
          tr = (-0.95, value, 1.0)  -- | top right
          bl = (-1.0, -1.0, 1.0)    -- | bottom left
          br = (-0.95, -1.0, 1.0)   -- | bottom right