{-# LANGUAGE FlexibleContexts #-}

module Collision.AABB(
    Collision(..)
) where

import Graphics.UI.GLUT hiding (None)
import Prelude          hiding (fst, id, lookup, snd, map, Right, Left)
import Data.List        hiding (insert, lookup, map)
import Data.IORef
import Control.Monad
import Text.Printf
import Data.Map as M
import Data.Set as S

import GameObjects.Objects.Ball     as Ball

import Collision.VectorOperations

data Collision = Left | Right | Top | Under | None
    deriving (Eq, Show)