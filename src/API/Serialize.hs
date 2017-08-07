module API.Serialize where

import Data.Aeson

import GameObjects.Ball
import GameObjects.Polygon

instance ToJSON Person where
 toJSON (Ball id center velocity radius score lastFloor) =
    object [ "id"        .= id
           , "center"    .= center
           , "velocity"  .= velocity
           , "radius"    .= radius
           , "score"     .= score
           , "lastFloor" .= lastFloor
            ]