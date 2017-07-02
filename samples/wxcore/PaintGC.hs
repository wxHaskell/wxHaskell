{--------------------------------------------------------------------------------
  Copyright 2003, Daan Leijen
  Paint demo, with antialiased drawing via GraphicsContext (wxGCDC).
  Antialiased drawing added by Dmitriy Nikitinskiy (2010).
--------------------------------------------------------------------------------}
module Main where

import Control.Monad
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcTypes

main :: IO ()
main
  = run gui

gui :: IO ()
gui
  = do -- create top frame
       f <- frameCreateTopFrame "Paint demo (with antialiasing)"

       -- for good measure: put a scrolled window inside the frame
       -- note that 'wxNO_FULL_REPAINT_ON_RESIZE'  is needed to prevent flicker on resize.
       s <- scrolledWindowCreate f idAny rectNull (wxHSCROLL + wxVSCROLL + wxNO_FULL_REPAINT_ON_RESIZE + wxCLIP_CHILDREN)

       -- virtual size is 20*40 = 800 pixels
       scrolledWindowSetScrollbars s 20 20 40 40 0 0 False
       
       -- to show the effect of double-buffering, we track the mouse with a small disc.
       mouseXY <- varCreate (pt 0 0)
       windowOnMouse s True {- get motion events -} (onMouse s mouseXY)

       -- set paint event handler:
       windowOnPaint s (\dc -> onPaint mouseXY (objectCast dc))

       -- show the frame
       _ <- windowShow f
       windowRaise f
       return ()
  where
    -- update the mouse position and force a repaint
    onMouse w mouseXY mouse
      = do varSet mouseXY (mousePos mouse)
           windowRefresh w False {- erase background -}

    -- do some painting.
    onPaint :: Var Point -> WindowDC a -> t -> IO ()
    onPaint mouseXY dc_ _view
      = -- first create some brushes and pens.
        withBrushStyle (BrushStyle (BrushHatch HatchCross) red) $ \brushRedHatch ->
        withBrushStyle (BrushStyle BrushSolid red)  $ \brushRed ->
        withBrushStyle (BrushStyle BrushSolid white)  $ \brushWhite ->
        withPenStyle (penColored blue 5) $ \penMedBlue ->
        do -- dcClearRect dc view
           dc <- gcdcCreate dc_
           dcSetBrush dc brushWhite
           dcDrawRectangle dc (rect (pt 20 20) (sz 500 500))

           dcSetBrush dc brushRedHatch
           dcDrawCircle dc (pt 100 100) 50

           dcSetPen dc penMedBlue
           dcDrawRectangle dc (rect (pt 200 200) (sz 50 50))

           dcSetBrush dc brushRed
           dcDrawEllipticArc dc (rect (pt 100 200) (sz 50 100)) 45 135

           -- draw the mouse bullet
           xy <- varGet mouseXY
           dcDrawCircle dc xy 10

           drawPolygon dc [(pt 200 400),(pt 300 300),(pt 400 400)]
           dcDrawRotatedText dc "Polygon" (pt 200 370) 45

           -- fonts
           dcWithFontStyle dc fontSwiss{ _fontSize = 12, _fontWeight = WeightBold } $
            do dcDrawText dc "Swiss 12pt bold" (pt 50 270)
               dcWithFontStyle dc fontDefault{ _fontFamily = FontScript, _fontSize = 16} $
                dcDrawText dc "Hand writing 16pt" (pt 50 290)
               dcDrawText dc "Swiss 12pt bold" (pt 50 310)

           (Size w h) <- getTextExtent dc "label"
           dcDrawRectangle dc (rect (pt 450 350) (sz (w+10) (h+10)))
           dcDrawText dc "label" (pt 455 355)

           -- cap styles
           dcWithPenStyle dc (penDefault{ _penWidth = 20, _penCap = CapRound }) $
            dcDrawLine dc (pt 400 100) (pt 500 100)

           dcWithPenStyle dc (penDefault{ _penWidth = 20, _penCap = CapProjecting }) $
            dcDrawLine dc (pt 400 150) (pt 500 150)

           dcWithPenStyle dc (penDefault{ _penWidth = 20, _penCap = CapButt }) $
            dcDrawLine dc (pt 400 200) (pt 500 200)

           dcSetBrush dc nullBrush
           dcDrawEllipse dc (rect (pt 200 100) (sz 100 50))

           c <- gcdcGetGraphicsContext dc
--         p <- graphicsPathCreate c
           p <- graphicsContextCreatePath c
           graphicsPathAddCircle p (point 0 0) 40
           graphicsPathMoveToPoint p $ point 0 $ -40
           graphicsPathAddLineToPoint p (Point 0 40)
           graphicsPathMoveToPoint p (Point (-40) 0)
           graphicsPathAddLineToPoint p (Point 40 0)
           graphicsPathCloseSubpath p
           graphicsPathAddRectangle p (Rect (-20) (-10) 40 20)
           graphicsContextTranslate c 200 70
           graphicsContextStrokePath c p
           graphicsContextTranslate c 100 0
           
           graphicsContextDrawText c "Rotate" (Point 0 $ -40);
           
           graphicsContextTranslate c 0 75
           
           forM_ [0,30..360 :: Double] $ \angle -> do
               graphicsContextPushState c
               --wxImage::RGBValue val = wxImage::HSVtoRGB(wxImage::HSVValue(float(angle)/360, 1, 1));
               let a = round $ angle * 255 / 360
                   a1= round $ cos(angle*pi/360) * 255
                   a2= round $ sin(angle*pi/360/2) * 64
               
               graphicsContextTranslate c (1.5 * 40 * cos ( angle * 2 * pi / 360 ) ) (1.5 * 40 * sin( angle * 2 *pi/360) )
       
               graphicsContextRotate c (angle * 2 * pi / 360)
               
               withBrushStyle (BrushStyle BrushSolid (colorRGBA a a1 a2 (64 :: Int))) $ \br -> do
                 --dcSetBrush dc br
                 graphicsContextSetBrush c br
                 withPenStyle (penColored (colorRGBA a a1 a2 128) 1) $ \pn -> do
                   --dcSetPen dc pn
                   graphicsContextSetPen c pn
                   graphicsContextDrawPath c p 1

               graphicsContextPopState c

           graphicsPathDelete p  
           gcdcDelete dc
