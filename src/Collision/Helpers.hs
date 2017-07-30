module Collision.Helpers where

import Graphics.UI.GLUT
import Data.IORef
import Control.Monad

-- | Atomically modifies the contents of an IORef.
--   Used instead ($~!) [GLUT]
(^&) :: IORef a -> (a -> a) -> IO ()
(^&) ref fun = do atomicModifyIORef' ref (\r -> (fun r, ()))