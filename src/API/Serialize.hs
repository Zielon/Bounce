module API.Serialize where

import Data.Aeson

import GameObjects.Objects.Ball

import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

serialize path = I.writeFile path (encodeToLazyText  (Ball 14 (0.4, 0.4) (0.0, 0.0) 0.1 0 0))