module API.Environment where
 
import Graphics.UI.GLUT
 
setProjection :: IO ()
setProjection = do
  matrixMode $= Projection
  ortho (-1) 1 (-1) 1 0 (-1)
 
grey1,grey9,red,white :: Color4 GLfloat
grey1 = Color4 0.1 0.1 0.1 1
grey9 = Color4 0.9 0.9 0.9 1
red   = Color4 1   0   0   1
white = Color4 1   1   1   1
 
setLights :: IO ()
setLights = do
  let l = Light 0
  ambient  l $= grey1
  diffuse  l $= white
  specular l $= white
  position l $= Vertex4 (-15) 1 1 (2 :: GLfloat)
  light    l $= Enabled
  lighting   $= Enabled

setMaterial :: IO ()
setMaterial = do
  materialAmbient   Front $= grey1
  materialDiffuse   Front $= red
  materialSpecular  Front $= grey9
  materialShininess Front $= (32 :: GLfloat)