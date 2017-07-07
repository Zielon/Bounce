module Keys where

import Graphics.UI.GLUT
import Data.IORef
import Data.Bool

getKeys :: IO [IORef (SpecialKey, Bool)]
getKeys = sequence [newIORef (KeyLeft, False), newIORef (KeyRight, False), newIORef (KeyUp, False), newIORef (KeyDown, False)]

(!!@) :: [IORef (SpecialKey, Bool)] -> SpecialKey -> IO (SpecialKey, Bool)
(!!@) (x:xs) key =
    pair >>= \(k, s) -> if key == k then return (k,s) else (!!@) xs key
    where pair = readIORef x

update :: [IORef (SpecialKey, Bool)] -> Int -> Bool -> IO ()
update arr i s = do
    modifyIORef (arr !! i) $ \(k, s') -> (k, s) 

getStatus :: [IORef (SpecialKey, Bool)] -> SpecialKey -> IO Bool
getStatus [] _       = return False
getStatus (x:xs) key = do
    pair <- (readIORef x)
    if (fst pair) == key then return $ snd pair
    else
        getStatus xs key

resetAll :: [IORef (SpecialKey, Bool)] -> IO ()
resetAll [] = return ()
resetAll (x:xs) = do
    modifyIORef x $ \(k, s) -> (k, False)
    resetAll xs