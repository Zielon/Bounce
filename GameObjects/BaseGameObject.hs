module GameObject.BaseGameObject where

import GameObjects.Global

-- | The general class instantiated by every game object
-- |
class Object a where
    setOffset   :: Vector -> a -> a
    setVelocity :: Vector -> a -> a
    draw        :: a -> IO ()