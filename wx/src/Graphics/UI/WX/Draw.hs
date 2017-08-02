{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
{-|
Module      :  Draw
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Drawing.

A /Device Context/ or 'DC', is an instance of 'Drawn', 'Brushed',
'Literate', and 'Colored'.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Draw
    ( 
    -- * Classes
      Drawn, pen, penKind, penWidth, penCap, penJoin, penColor
    , Brushed, brush, brushKind, brushColor
    -- * Types
    , DC, Bitmap
      -- * Drawing
    , circle, arc, ellipse, ellipticArc
    , line, polyline, polygon
    , drawPoint, drawRect, roundedRect
    , drawText, rotatedText, drawBitmap, drawImage
    -- * Internal
    , dcWith, dcClear
    ) where

import Graphics.UI.WXCore  hiding (center, point)


import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes    hiding (text)



{--------------------------------------------------------------------------------

--------------------------------------------------------------------------------}
class Drawn w where
  pen       :: Attr w PenStyle
  penKind   :: Attr w PenKind  
  penWidth  :: Attr w Int
  penCap    :: Attr w CapStyle
  penJoin   :: Attr w JoinStyle
  penColor  :: Attr w Color

class Brushed w where
  brush      :: Attr w BrushStyle
  brushKind  :: Attr w BrushKind
  brushColor :: Attr w Color

instance Drawn (DC a) where
  pen
    = newAttr "pen" dcGetPenStyle dcSetPenStyle
  
  penKind
      = mapAttr _penKind (\pstyle x -> pstyle{ _penKind = x }) pen

  penWidth
    = mapAttr _penWidth (\pstyle x -> pstyle{ _penWidth = x }) pen

  penCap
    = mapAttr _penCap (\pstyle x -> pstyle{ _penCap = x }) pen
  
  penJoin
    = mapAttr _penJoin (\pstyle x -> pstyle{ _penJoin = x }) pen

  penColor
    = mapAttr _penColor (\pstyle color_ -> pstyle{ _penColor = color_ }) pen

instance Brushed (DC a) where
  brush
    = newAttr "brush" dcGetBrushStyle dcSetBrushStyle

  brushKind
    = mapAttr _brushKind (\bstyle x -> bstyle{ _brushKind = x }) brush

  brushColor
    = mapAttr _brushColor (\bstyle color_ -> bstyle{ _brushColor = color_ }) brush

instance Literate (DC a) where
  font
    = newAttr "font" dcGetFontStyle dcSetFontStyle

  textColor
    = newAttr "textcolor" dcGetTextForeground dcSetTextForeground

  textBgcolor
    = newAttr "textbgcolor" dcGetTextBackground dcSetTextForeground

instance Colored (DC a) where
  color
    = newAttr "color" (\dc -> get dc penColor) (\dc c -> set dc [penColor := c, textColor := c])

  bgcolor
    = newAttr "bgcolor" (\dc -> get dc brushColor) (\dc c -> set dc [brushColor := c, textBgcolor := c])


-- Save pen/font/brush efficiently.
dcWith :: DC a -> [Prop (DC a)] -> IO b -> IO b
dcWith dc props io
  | null props = io
  | otherwise  = dcEncapsulate dc (do set dc props; io)

-- | Draw a circle given a center point and radius.
circle :: DC a -> Point -> Int -> [Prop (DC a)] -> IO ()
circle dc center radius props
  = dcWith dc props (dcDrawCircle dc center radius)

-- | Draw an arc of a circle. Takes the center of the circle, 
-- its radius and a starting and ending point relative to the
-- three-o\'clock position. Angles are in degrees and positive
-- values denote a counter clockwise motion. If the angles are
-- equal, an entire circle is drawn.
arc :: DC a -> Point -> Int -> Double -> Double -> [Prop (DC a)] -> IO ()
arc dc center radius start end props
  = ellipticArc dc bounds start end props
  where
    bounds 
      = rect (pt (pointX center - radius) (pointY center - radius)) (sz (2*radius) (2*radius))
{-
  = dcWith dc props (dcDrawArc dc center (point start) (point end) )
  where
    point angle
      = let radians = (2*pi*angle)/360
            x       = px center + round (cos radians * fromIntegral radius)
            y       = py center - round (sin radians * fromIntegral radius)
        in (pt x y)
-}

-- | Draw an ellipse, bounded by a certain rectangle.
ellipse :: DC a -> Rect -> [Prop (DC a)] -> IO ()
ellipse dc rect_ props
  = dcWith dc props (dcDrawEllipse dc rect_)

-- | Draw an elliptic arc. Takes the bounding rectangle, 
-- and a starting and ending point relative to the
-- three-o\'clock position from the center of the rectangle. 
-- Angles are in degrees and positive
-- values denote a counter clockwise motion. If the angles are
-- equal, an entire ellipse is drawn.
ellipticArc :: DC a -> Rect -> Double -> Double -> [Prop (DC a)] -> IO ()
ellipticArc dc rect_ start end props
  = dcWith dc props (dcDrawEllipticArc dc rect_ start end)

-- | Draw a line.
line :: DC a -> Point -> Point -> [Prop (DC a)] -> IO ()
line dc start end props
  = dcWith dc props (dcDrawLine dc start end)

-- | Draw a polyline.
polyline :: DC a -> [Point] -> [Prop (DC a)] -> IO ()
polyline dc points props
  = dcWith dc props (drawLines dc points)

-- | Draw a polygon. The polygon is filled with the odd-even rule.
-- Note that the polygon is automatically closed.
polygon :: DC a -> [Point] -> [Prop (DC a)] -> IO ()
polygon dc points props
  = dcWith dc props (drawPolygon dc points)


-- | Draw a single point. 
drawPoint :: DC a -> Point -> [Prop (DC a)] -> IO ()
drawPoint dc center props
  = dcWith dc props (dcDrawPoint dc center)

-- | Draw a rectangle.
drawRect :: DC a -> Rect -> [Prop (DC a)] -> IO ()
drawRect dc rect_ props
  = dcWith dc props (dcDrawRectangle dc rect_)

-- | Draw a rectangle with rounded corners. The corners are
-- quarter circles with the given radius.
-- If radius is positive, the value is assumed to be the radius of the rounded corner. 
-- If radius is negative, the absolute value is assumed to be the proportion of the smallest 
-- dimension of the rectangle. This means that the corner can be a sensible size relative to 
-- the size of the rectangle, and also avoids the strange effects X produces when the corners 
-- are too big for the rectangle.
roundedRect :: DC a -> Rect -> Double -> [Prop (DC a)] -> IO ()
roundedRect dc rect_ radius props
  = dcWith dc props (dcDrawRoundedRectangle dc rect_ radius)

-- | Draw text.
drawText :: DC a -> String -> Point -> [Prop (DC a)] -> IO ()
drawText dc text point props
  = dcWith dc props (dcDrawText dc text point)

-- | Draw rotated text. Takes an angle in degrees relative to the
-- three-o\'clock position.
rotatedText :: DC a -> String -> Point -> Double -> [Prop (DC a)] -> IO ()
rotatedText dc text point angle props
  = dcWith dc props (dcDrawRotatedText dc text point angle)

-- | Draw a bitmap. Takes a bitmap, a point and a boolean
-- that is 'True' when the bitmap is drawn with a transparency mask.
drawBitmap :: DC a -> Bitmap () -> Point -> Bool -> [Prop (DC a)] -> IO ()
drawBitmap dc bitmap point transparent props
  = if bitmap == nullBitmap || objectIsNull bitmap 
     then return ()
     else do ok <- bitmapIsOk bitmap
             if not ok 
              then return ()
              else dcWith dc props (dcDrawBitmap dc bitmap point transparent)

-- | Draw an image. 
drawImage :: DC a -> Image b -> Point -> [Prop (DC a)] -> IO ()
drawImage dc image pt_ props
  = do bm <- bitmapCreateFromImage image (-1)
       drawBitmap dc bm pt_ False props
       bitmapDelete bm
