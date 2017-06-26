module MapGenerator where

import Graphics.UI.GLUT

getBottom :: [(GLfloat,GLfloat,GLfloat)]
getBottom = [(-1.0,-0.95,0.0),(1.0,-0.95,0.0)]

generate :: [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))]
generate = [((-1.0,-0.5,0.0), (-0.5,-0.5,0.0)),  
            ((-0.4,0.5,0.0), (-0.1,0.5,0.0)),
            ((0.6,0.8,0.0), (1.0,0.8,0.0))]


flatten :: [((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat))] -> [(GLfloat,GLfloat,GLfloat)]
flatten lines = flatten' lines []
                    where
                        flatten' []     points = points
                        flatten' (x:xs) points = 
                            flatten' xs (points ++ [f] ++ [s])
                            where f = fst x
                                  s = snd x