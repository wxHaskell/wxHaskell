{- 
  Demos multiple OpenGL canvas's

  Sample contributed by Patrick Scheibe 
-}
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


defaultWidth  :: Int
defaultWidth  = 320
defaultHeight :: Int
defaultHeight = 200

gui :: IO ()
gui = do
   f <- frame [ text := "Simple OpenGL" ]

-- We just create two glCanvas

   glCanvas <- glCanvasCreateEx f 0 (Rect 0 0 defaultWidth defaultHeight)
        0 "GLCanvas" [GL_RGBA] nullPalette
   glCanvas2 <- glCanvasCreateEx f 0 (Rect 0 0 defaultWidth defaultHeight)
        0 "GLCanvas" [GL_RGBA] nullPalette

   let glWidgetLayout = fill $ row 5 [widget glCanvas2,  widget glCanvas]

-- Hint: You have to use the paintRaw event. For switching between the two
--   glwindows you can give both of them as parameter
   WX.set f [ layout := glWidgetLayout
            , on paintRaw := paintGL glCanvas glCanvas2
            ]


convWG :: Size2D Int -> GL.Size
convWG (WX.Size w h) = (GL.Size (convInt32  w) (convInt32  h))

convInt32 :: Int -> GLsizei
convInt32 = fromInteger . toInteger

paintGL :: GLCanvas a -> GLCanvas a -> DC b -> WX.Rect -> [WX.Rect]-> IO ()
paintGL gl1 gl2 _dc myrect _ = do

-- Now we switch to the first one
-- and do all init and painting stuff
-- Hint: I changed the backgroundcolor for clearance

   glContext1 <- glContextCreateFromNull gl1
   _ <- glCanvasSetCurrent gl1 glContext1
   myInit
   reshape $ convWG $ rectSize myrect
   GL.clearColor GL.$= GL.Color4 1 0 0 0
   display
   _ <- glCanvasSwapBuffers gl1

-- All the same for the second one
   glContext2 <- glContextCreateFromNull gl2
   _ <- glCanvasSetCurrent gl2 glContext2
   myInit
   reshape $ convWG $ rectSize myrect
   GL.clearColor GL.$= GL.Color4 0 2 0 0
   display
   _ <- glCanvasSwapBuffers gl2
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
--   GL.clearColor GL.$= GL.Color4 1 0 0 0
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
