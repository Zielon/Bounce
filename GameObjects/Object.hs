module GameObject.Object where

import GameObjects.Global

-- | The general class instantiated by every map object
-- |
class Object a where
    setOffset   :: Vector -> a -> a
    setVelocity :: Vector -> a -> a
    draw        :: a -> IO ()