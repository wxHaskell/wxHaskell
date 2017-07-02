{-
   Author :  shelarcy 2004
   Advised by: Sean Seefried

   Adapted from: BezCurve.hs
   By: (c) Sven Panne 2003 <sven_panne@yahoo.com>
   
   "BezCurve.hs (adapted from fog.c which is (c) Silicon Graphics, Inc)
   This file is part of HOpenGL and distributed under a BSD-style license
   See the file libraries/GLUT/LICENSE"

   This program renders a lighted, filled Bezier surface, using two-dimensional
   evaluators.
-}

-- ghci -package wx -package OpenGL

module Main
where
    
import Data.List ( transpose )
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Rendering.OpenGL
-- Many code and Type are ambiguous, so we must qualify names.
import qualified Graphics.UI.WX as WX
import qualified Graphics.Rendering.OpenGL as GL

main :: IO()
main = start gui


defaultWidth :: Int
defaultWidth  = 320
defaultHeight :: Int
defaultHeight = 200

gui :: IO()
gui = do
   f <- frame [ text := "Simple OpenGL" ]
   glCanvas <- glCanvasCreateEx f 0 (Rect 0 0 defaultWidth defaultHeight) 0 "GLCanvas" [GL_RGBA] nullPalette
   glContext <- glContextCreateFromNull glCanvas
   _ <- glCanvasSetCurrent glCanvas glContext
   let glWidgetLayout = fill $ widget glCanvas
   WX.set f [ layout := glWidgetLayout
-- you have to use the paintRaw event. Otherwise the OpenGL window won't
-- show anything!
            , on paintRaw := paintGL glCanvas
            ]
   repaint f

convWG :: Size2D Int -> GL.Size
convWG (WX.Size w h) = (GL.Size (convInt32  w) (convInt32  h))

convInt32 :: Int -> GLsizei
convInt32 = fromInteger . toInteger

-- This paint function gets the current glCanvas for knowing where to draw in.
-- It is possible to have multiple GL windows in your application.
paintGL :: GLCanvas a -> DC b -> WX.Rect -> [WX.Rect]-> IO ()
paintGL glWindow _dc myrect _ = do
   myInit
   reshape $ convWG $ rectSize myrect
   display
   _ <- glCanvasSwapBuffers glWindow
   return ()


ctrlPoints :: [[GL.Vertex3 GL.GLfloat]]
ctrlPoints = [
   [ GL.Vertex3 (-1.5) (-1.5)   4.0,  GL.Vertex3 (-0.5) (-1.5)   2.0,
     GL.Vertex3   0.5  (-1.5) (-1.0), GL.Vertex3   1.5  (-1.5)   2.0 ],
   [ GL.Vertex3 (-1.5) (-0.5)   1.0,  GL.Vertex3 (-0.5) (-0.5)   3.0,
     GL.Vertex3   0.5  (-0.5)   0.0,  GL.Vertex3   1.5  (-0.5) (-1.0) ],
   [ GL.Vertex3 (-1.5)   0.5    4.0,  GL.Vertex3 (-0.5)   0.5    0.0,
     GL.Vertex3   0.5    0.5    3.0,  GL.Vertex3   1.5    0.5    4.0 ],
   [ GL.Vertex3 (-1.5)   1.5  (-2.0), GL.Vertex3 (-0.5)   1.5  (-2.0),
     GL.Vertex3   0.5    1.5    0.0,  GL.Vertex3   1.5    1.5  (-1.0) ]]

initlights :: IO ()
initlights = do
   GL.lighting GL.$= GL.Enabled
   GL.light (GL.Light 0) GL.$= GL.Enabled

   GL.ambient  (GL.Light 0) GL.$= GL.Color4 0.2 0.2 0.2 1.0
   GL.position (GL.Light 0) GL.$= GL.Vertex4 0 0 2 1

   GL.materialDiffuse   GL.Front GL.$= GL.Color4 0.6 0.6 0.6 1.0
   GL.materialSpecular  GL.Front GL.$= GL.Color4 1.0 1.0 1.0 1.0
   GL.materialShininess GL.Front GL.$= 50

myInit :: IO ()
myInit = do
   GL.clearColor GL.$= GL.Color4 0.1 0.1 0.6 0
   GL.depthFunc GL.$= Just GL.Less
   m <- GL.newMap2 (0, 1) (0, 1) (transpose ctrlPoints)
   GL.map2 GL.$= Just (m :: GLmap2 GL.Vertex3 GL.GLfloat)
   GL.autoNormal GL.$= GL.Enabled
   mapGrid2 GL.$= ((20, (0, 1)), (20, (0, 1 :: GL.GLfloat)))
   initlights  -- for lighted version only

display :: IO ()
display = do
   GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
   GL.preservingMatrix $ do
     GL.rotate (85 :: GL.GLfloat) (GL.Vector3 1 1 1)
     evalMesh2 Fill (0, 20) (0, 20)
   GL.flush

reshape :: GL.Size -> IO ()
reshape mysize@(GL.Size w h) = do
   GL.viewport GL.$= (GL.Position 0 0, mysize)
   GL.matrixMode GL.$= GL.Projection
   GL.loadIdentity
   let wf = fromIntegral w
       hf = fromIntegral h
   if w <= h
      then GL.ortho (-4.0) 4.0 (-4.0*hf/wf) (4.0*hf/wf) (-4.0) 4.0
      else GL.ortho (-4.0*wf/hf) (4.0*wf/hf) (-4.0) 4.0 (-4.0) 4.0
   GL.matrixMode GL.$= GL.Modelview 0
   GL.loadIdentity

