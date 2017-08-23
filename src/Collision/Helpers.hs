module Collision.Helpers where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Control.Parallel.Strategies
import Data.Map

import GameObjects.GameObject
import GameObjects.Objects.Ball

-- | Atomically modifies the contents of an IORef.
--   Used instead ($~!) [GLUT]
(^&) :: IORef a -> (a -> a) -> IO ()
(^&) ref fun = do atomicModifyIORef' ref (\r -> (fun r, ()))

(#-) :: GameObject -> Map Int GameObject -> Map Int GameObject
(#-) (GameObject object) dictionary = 
    insert i (GameObject object) dictionary
    where i = getId object

maxRealFloat :: RealFloat a => a -> a
maxRealFloat x = encodeFloat b (e-1) `asTypeOf` x where
  b      = floatRadix x - 1
  (_, e) = floatRange x

infinity :: RealFloat a => a
infinity = if isInfinite inf then inf else maxRealFloat 1.0 
    where inf = 1/0
