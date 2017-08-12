module GameObjects.Objects.Segment(
    Ray(..),
    Segment(..),
    Vector
) where

import Graphics.UI.GLUT
import GameObjects.GameObject

class Ray a where
    draw :: a -> IO ()

data Segment = Segment {
    lineColor  :: (Float, Float, Float),
    start      :: Vector,
    end        :: Vector
}

instance Ray Segment where
    draw segment = do
        getColor3f r g b
        renderPrimitive Lines $ mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) [start segment, end segment]
        where (r, g, b) = lineColor segment