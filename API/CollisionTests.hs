module API.CollisionTests(
    Collision(..),
    testAABB,
    gridIntersect2D
) where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (fst, id, lookup, snd, map, Right, Left)
import Data.List        hiding (insert, lookup, map)
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map as M
import Data.Set as S

import GameObjects.Positionable
import GameObjects.Floor
import GameObjects.Ball
import Engines.FloorEngine

data Collision = Left | Right | Over | Under | None
    deriving (Eq, Show)

-- AABB test for games objects
testAABB :: (Positionable a, Positionable b) => a -> b -> Collision
testAABB a b =
    if      d1x > 0.0 || d1y > 0.0 then None
    else if d2x > 0.0 || d2y > 0.0 then None
    else if b_max_y - a_min_y > 0.0 && b_min_y < a_min_y && (d1x < 0.0 || d2x < 0.0) then Over  -- a over  b
    else if a_max_y - b_min_y > 0.0 && a_max_y < b_max_y && (d1x < 0.0 || d2x < 0.0) then Under -- a under b
    else if b_max_x - a_min_x > 0.0 && a_max_x > b_max_x && (d1y < 0.0 || d1y < 0.0) then Right -- a on right b
    else if a_max_x - b_min_x > 0.0 && a_min_x < b_min_x && (d1y < 0.0 || d1y < 0.0) then Left  -- a on left b
    else None
    where (a_min_x, a_min_y) = getMin a
          (a_max_x, a_max_y) = getMax a
          (b_min_x, b_min_y) = getMin b
          (b_max_x, b_max_y) = getMax b
          d1x = b_min_x - a_max_x
          d1y = b_min_y - a_max_y
          d2x = a_min_x - b_max_x
          d2y = a_min_y - b_max_y

gridIntersect2D :: IORef (Map Int Floor) -> IO ()
gridIntersect2D dictionary = do
    floors <- get dictionary
    grid   <- newIORef $ (M.fromList [] :: Map (Int, Int) [Int])
    forM_ floors $ \a -> do
        g  <- get grid
        let i = (id a)
            (a_min_x, a_min_y) = getMin a
            (a_max_x, a_max_y) = getMax a

        forM_ [Prelude.floor(a_min_x/h)..Prelude.ceiling(a_max_x/h)] $ \x ->
            forM_ [Prelude.floor(a_min_y/h)..Prelude.ceiling(a_max_y/h)] $ \y -> do
                case lookup (x,y) g of
                    Nothing    -> grid $~! (\d -> insertWith (++) (x,y) [i] d)    -- Not exist
                    Just cells ->
                        do grid $~! (\d -> insertWith (++) (x,y) [i] d)           -- Update the list on the (x,y) position in the map
                           forM_ cells $ \n -> do
                             case lookup n floors of
                                Nothing -> error "Wrong floor ID"
                                Just b  -> action a b

    where h          = 0.01 :: GLfloat
          width      = 0.05 :: GLfloat
          action :: Floor -> Floor -> IO ()
          action a b = do
                let result = [(a, b, (testAABB a b)), (b, a, (testAABB b a))]
                forM_ result $ \(f, s, r) -> do
                    let (min_x, min_y) = getMin f
                        (max_x, max_y) = getMax f
                    case r of
                        Over  -> dictionary $~! (\d -> M.insert (id s) (setY (min_y - width) (min_y) s) d)
                        Under -> dictionary $~! (\d -> M.insert (id s) (setY (max_y) (max_y + width) s) d)
                        _     -> return ()