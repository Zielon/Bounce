module Ball where

import Graphics.UI.GLUT

data Ball = Ball {
    getX         :: GLFloat,
    getY         :: GLFloat,
    getVelocityX :: GLFloat,
    getVelocityY :: GLFloat
}