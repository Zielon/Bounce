module Collision.Helpers where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
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