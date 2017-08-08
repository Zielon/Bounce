module API.Serialize where

import Data.Aeson

import GameObjects.Objects.Ball
import GameObjects.Objects.Polygon

import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

-- (Ball 14 (0.4, 0.4) (0.0, 0.0) 0.1 0 0) 

serialize path = I.writeFile path (encodeToLazyText (GamePolygon 3 (0.0, 0.0) [(-0.8, -0.8), (-0.8, -0.7),(0.8, -0.7), (0.8, -0.8)]))