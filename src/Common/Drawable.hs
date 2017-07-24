{-# LANGUAGE ExistentialQuantification #-}

module Common.Drawable where

class Drawable_ a where
    render :: a -> IO ()

data Drawable = forall a. Drawable_ a => Drawable a