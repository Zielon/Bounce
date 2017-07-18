module API.Keys(
    GameKey(..),
    getKeys,
    updateKey
) where

import Graphics.UI.GLUT
import Data.IORef
import Data.Bool
import Data.Map

data GameKey = GameKeyLeft | GameKeyRight | GameKeyForce | GameKeyOne | GameKeyTwo | GameKeyThree | GameKeyFour
    deriving (Eq, Ord)

getKeys :: Map GameKey Bool
getKeys = fromList [ (GameKeyOne,   False),
                     (GameKeyTwo,   False),
                     (GameKeyThree, False),
                     (GameKeyFour,  False),
                     (GameKeyLeft,  False),
                     (GameKeyRight, False), 
                     (GameKeyForce, False)]

updateKey :: IORef (Map GameKey Bool) -> GameKey -> Bool -> IO ()
updateKey ref key value = do ref $~! (\d -> insert key value d)

resetAll :: IORef (Map GameKey Bool) -> IO ()
resetAll ref = do ref $~! (\d -> Data.Map.map (\f -> False) d)