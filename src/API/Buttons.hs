module API.Buttons(
    GameButton(..),
    getButtons,
    updateButton
) where

import Graphics.UI.GLUT
import Data.IORef
import Data.Bool
import Data.Map

import Collision.Helpers

data GameButton = GameButtonLeft | GameButtonRight
    deriving (Eq, Ord)

getButtons :: Map GameButton Bool
getButtons = fromList [ (GameButtonLeft,  False),
                        (GameButtonRight, False)]

updateButton :: IORef (Map GameButton Bool) -> GameButton -> Bool -> IO ()
updateButton ref key value = do ref ^& (\d -> insert key value d)

resetAll :: IORef (Map GameButton Bool) -> IO ()
resetAll ref = do ref ^& (\d -> Data.Map.map (\f -> False) d)