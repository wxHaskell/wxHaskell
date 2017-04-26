-----------------------------------------------------------------------------------------
{-|
Module      :  Draw
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Drawing.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Draw
        (
        -- * DC
          drawLines, drawPolygon, getTextExtent, getFullTextExtent, dcClearRect
        -- ** Creation
        , withPaintDC, withClientDC, dcDraw
        , withSVGFileDC, withSVGFileDCWithSize, withSVGFileDCWithSizeAndResolution
        -- ** Draw state
        , DrawState, dcEncapsulate, dcGetDrawState, dcSetDrawState, drawStateDelete
        -- ** Double buffering
        , dcBuffer, dcBufferWithRef, dcBufferWithRefEx
        , dcBufferWithRefExGcdc
        -- * Scrolled windows
        , windowGetViewStart, windowGetViewRect, windowCalcUnscrolledPosition
        -- * Font
        , FontStyle(..), FontFamily(..), FontShape(..), FontWeight(..)
        , fontDefault, fontSwiss, fontSmall, fontItalic, fontFixed
        , withFontStyle, dcWithFontStyle
        , dcSetFontStyle, dcGetFontStyle
        , fontCreateFromStyle, fontGetFontStyle
        -- * Brush
        , BrushStyle(..), BrushKind(..)
        , HatchStyle(..)
        , brushDefault, brushSolid, brushTransparent
        , dcSetBrushStyle, dcGetBrushStyle
        , withBrushStyle, dcWithBrushStyle, dcWithBrush
        , brushCreateFromStyle, brushGetBrushStyle
        -- * Pen
        , PenStyle(..), PenKind(..), CapStyle(..), JoinStyle(..), DashStyle(..)
        , penDefault, penColored, penTransparent
        , dcSetPenStyle, dcGetPenStyle
        , withPenStyle, dcWithPenStyle, dcWithPen
        , penCreateFromStyle, penGetPenStyle
        ) where

import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcClassInfo
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Defines

import Foreign.Storable
import Foreign.Marshal.Alloc


{--------------------------------------------------------------------------------
  DC creation
--------------------------------------------------------------------------------}
-- | Safely perform a drawing operation on a DC.
dcDraw :: DC a -> IO b -> IO b
dcDraw dc io
  = bracket_ (do dcSetPenStyle dc penDefault
                 dcSetBrushStyle dc brushDefault) 
             (do dcSetPen dc nullPen
                 dcSetBrush dc nullBrush) 
             io

-- | Use a 'PaintDC'.
-- Draw on a window within an 'on paint' event.
withPaintDC :: Window a -> (PaintDC () -> IO b) -> IO b
withPaintDC window draw
  = bracket (paintDCCreate window) (paintDCDelete) (\dc -> dcDraw dc (draw dc))

-- | Use a 'ClientDC'.
-- Draw on a window from outside an 'on paint' event.
withClientDC :: Window a -> (ClientDC () -> IO b) -> IO b
withClientDC window draw
  = bracket (clientDCCreate window) (clientDCDelete) (\dc -> dcDraw dc (draw dc))

-- | Use a 'SVGFileDC'.
withSVGFileDC :: FilePath -> (SVGFileDC () -> IO b) -> IO b
withSVGFileDC fname draw
  = bracket (svgFileDCCreate fname) (svgFileDCDelete) (\dc -> dcDraw dc (draw dc))

withSVGFileDCWithSize :: FilePath -> Size -> (SVGFileDC () -> IO b) -> IO b
withSVGFileDCWithSize fname size draw
  = bracket (svgFileDCCreateWithSize fname size) (svgFileDCDelete) (\dc -> dcDraw dc (draw dc))

withSVGFileDCWithSizeAndResolution :: FilePath -> Size -> Float -> (SVGFileDC () -> IO b) -> IO b
withSVGFileDCWithSizeAndResolution fname size dpi draw
  = bracket (svgFileDCCreateWithSizeAndResolution fname size dpi) (svgFileDCDelete) (\dc -> dcDraw dc (draw dc))


-- | Clear a specific rectangle with the current background brush.
-- This is preferred to 'dcClear' for scrolled windows as 'dcClear' sometimes
-- only clears the original view area, instead of the currently visible scrolled area.
-- Unfortunately, the background brush is not set correctly on wxMAC 2.4, and 
-- this will always clear to a white color on mac systems.
dcClearRect :: DC a -> Rect -> IO ()
dcClearRect dc r
  = bracket (dcGetBackground dc)
            (brushDelete)
            (\brush -> dcWithBrush dc brush $
                         dcWithPenStyle dc penTransparent $ 
                           dcDrawRectangle dc r)

{-
-- | Fill a rectangle with a certain color.
dcFillRect :: DC a -> Rect -> Color -> IO ()
dcFillRect dc r color
  = dcWithBrushStyle dc (brushSolid color) $
     dcWithPenStyle dc penTransparent $
      dcDrawRectangle dc r
-}

{--------------------------------------------------------------------------------
  Windows
--------------------------------------------------------------------------------}
-- | Get logical view rectangle, adjusted for scrolling.
windowGetViewRect :: Window a -> IO Rect
windowGetViewRect window
  = do size <- windowGetClientSize window
       org  <- windowGetViewStart window
       return (rect org size)

-- | Get logical view start, adjusted for scrolling.
windowGetViewStart :: Window a -> IO Point
windowGetViewStart window
  = do isScrolled <- objectIsScrolledWindow window
       -- adjust coordinates for a scrolled window
       if (isScrolled)
        then do let scrolledWindow = objectCast window
                (Point sx sy) <- scrolledWindowGetViewStart scrolledWindow
                (Point w h)   <- scrolledWindowGetScrollPixelsPerUnit scrolledWindow
                return (Point (w*sx) (h*sy))
        else return pointZero

-- | Get logical coordinates adjusted for scrolling.
windowCalcUnscrolledPosition :: Window a -> Point -> IO Point
windowCalcUnscrolledPosition window p
  = do isScrolled <- objectIsScrolledWindow window
       -- adjust coordinates for a scrolled window
       if (isScrolled)
        then do let scrolledWindow = objectCast window
                scrolledWindowCalcUnscrolledPosition scrolledWindow p
        else return p

{--------------------------------------------------------------------------------
  Font
--------------------------------------------------------------------------------}


-- | Font descriptor. The font is normally specified thru the 'FontFamily', giving
-- some degree of portability. The '_fontFace' can be used to specify the exact (platform
-- dependent) font. 
--
-- Note that the original wxWidgets @FontStyle@ is renamed to @FontShape@.
data FontStyle
  = FontStyle{ _fontSize      :: !Int
             , _fontFamily    :: !FontFamily
             , _fontShape     :: !FontShape
             , _fontWeight    :: !FontWeight
             , _fontUnderline :: !Bool
             , _fontFace      :: !String       -- ^ normally @\"\"@
             , _fontEncoding  :: !Int          -- ^ normally @wxFONTENCODING_DEFAULT@
             }
  deriving (Eq,Show)

-- | Default 10pt font.
fontDefault :: FontStyle
fontDefault
  = FontStyle 10 FontDefault ShapeNormal WeightNormal False "" wxFONTENCODING_DEFAULT

-- | Default 10pt sans-serif font.
fontSwiss :: FontStyle
fontSwiss
  = fontDefault{ _fontFamily = FontSwiss }

-- | Default 8pt font.
fontSmall :: FontStyle
fontSmall
  = fontDefault{ _fontSize = 8 }

-- | Default 10pt italic.
fontItalic :: FontStyle
fontItalic
  = fontDefault{ _fontShape = ShapeItalic }

-- | Monospaced font, 10pt.
fontFixed :: FontStyle
fontFixed 
  = fontDefault{ _fontFamily = FontModern }

-- | Standard font families.
data FontFamily
  = FontDefault       -- ^ A system default font.
  | FontDecorative    -- ^ Decorative font.
  | FontRoman         -- ^ Formal serif font.
  | FontScript        -- ^ Hand writing font.
  | FontSwiss         -- ^ Sans-serif font.
  | FontModern        -- ^ Fixed pitch font.
  | FontTeletype      -- ^ A teletype (i.e. monospaced) font
  deriving (Eq,Show)

-- | The font style.
data FontShape
  = ShapeNormal
  | ShapeItalic
  | ShapeSlant
  deriving (Eq,Show)

-- | The font weight.
data FontWeight
  = WeightNormal
  | WeightBold
  | WeightLight
  deriving (Eq,Show)

-- | Use a font that is automatically deleted at the end of the computation.
withFontStyle :: FontStyle -> (Font () -> IO a) -> IO a
withFontStyle fontStyle f
  = do (font,delete) <- fontCreateFromStyle fontStyle
       finally (f font) delete


-- | Set a font that is automatically deleted at the end of the computation.
dcWithFontStyle :: DC a -> FontStyle -> IO b -> IO b
dcWithFontStyle dc fontStyle io
  = withFontStyle fontStyle $ \font ->
    bracket  (do oldFont <- dcGetFont dc
                 dcSetFont dc font
                 return oldFont)
             (\oldFont ->
              do dcSetFont dc oldFont   -- restore previous font
                 fontDelete oldFont)
             (const io)

-- | Set the font info of a DC.
dcSetFontStyle :: DC a -> FontStyle -> IO ()
dcSetFontStyle dc info
  = do (font,del) <- fontCreateFromStyle info
       finalize del $
        do dcSetFont dc font

-- | Get the current font info.
dcGetFontStyle :: DC a -> IO FontStyle
dcGetFontStyle dc
  = do font <- dcGetFont dc
       finalize (fontDelete font) $
        do fontGetFontStyle font


-- | Create a 'Font' from 'FontStyle'. Returns both the font and a deletion procedure.
fontCreateFromStyle :: FontStyle -> IO (Font (),IO ())
fontCreateFromStyle (FontStyle size family style weight underline face encoding)
  = do font <- fontCreate size cfamily cstyle cweight underline face encoding
       return (font,when (font /= objectNull) (fontDelete font))
  where
    cfamily
      = case family of
          FontDefault     -> wxFONTFAMILY_DEFAULT
          FontDecorative  -> wxFONTFAMILY_DECORATIVE
          FontRoman       -> wxFONTFAMILY_ROMAN
          FontScript      -> wxFONTFAMILY_SCRIPT
          FontSwiss       -> wxFONTFAMILY_SWISS
          FontModern      -> wxFONTFAMILY_MODERN
          FontTeletype    -> wxFONTFAMILY_TELETYPE
 
    cstyle
      = case style of
          ShapeNormal     -> wxFONTSTYLE_NORMAL
          ShapeItalic     -> wxFONTSTYLE_ITALIC
          ShapeSlant      -> wxFONTSTYLE_SLANT

    cweight
      = case weight of
          WeightNormal    -> wxFONTWEIGHT_NORMAL
          WeightBold      -> wxFONTWEIGHT_BOLD
          WeightLight     -> wxFONTWEIGHT_LIGHT


-- | Get the 'FontStyle' from a 'Font' object.
fontGetFontStyle :: Font () -> IO FontStyle
fontGetFontStyle font
  = if (objectIsNull font)
     then return fontDefault
     else do ok <- fontIsOk font
             if not ok
               then return fontDefault
               else do size    <- fontGetPointSize font
                       cfamily <- fontGetFamily font
                       cstyle  <- fontGetStyle font
                       cweight <- fontGetWeight font
                       cunderl <- fontGetUnderlined font
                       face    <- fontGetFaceName font
                       enc     <- fontGetEncoding font
                       return (FontStyle size (toFamily cfamily) (toStyle cstyle) 
                                (toWeight cweight) 
                                (cunderl /= 0) face enc)
   where
    toFamily f
      | f == wxFONTFAMILY_DECORATIVE   = FontDecorative
      | f == wxFONTFAMILY_ROMAN        = FontRoman
      | f == wxFONTFAMILY_SCRIPT       = FontScript
      | f == wxFONTFAMILY_SWISS        = FontSwiss
      | f == wxFONTFAMILY_MODERN       = FontModern
      | f == wxFONTFAMILY_TELETYPE     = FontTeletype
      | otherwise                      = FontDefault

    toStyle s
      | s == wxFONTSTYLE_ITALIC        = ShapeItalic
      | s == wxFONTSTYLE_SLANT         = ShapeSlant
      | otherwise                      = ShapeNormal

    toWeight w
      | w == wxFONTWEIGHT_BOLD         = WeightBold
      | w == wxFONTWEIGHT_LIGHT        = WeightLight
      | otherwise                      = WeightNormal


{--------------------------------------------------------------------------------
  Pen
--------------------------------------------------------------------------------}

-- | Pen style.
data PenStyle
  = PenStyle { _penKind  :: !PenKind
             , _penColor :: !Color
             , _penWidth :: !Int
             , _penCap   :: !CapStyle
             , _penJoin  :: !JoinStyle 
             }
  deriving (Eq,Show)

-- | Pen kinds.
data PenKind
  = PenTransparent    -- ^ No edge.
  | PenSolid
  | PenDash   { _penDash   :: !DashStyle  }
  | PenHatch  { _penHatch  :: !HatchStyle }
  | PenStipple{ _penBitmap :: !(Bitmap ())}    -- ^ @_penColor@ is ignored
  deriving (Eq,Show)

-- | Default pen (@PenStyle PenSolid black 1 CapRound JoinRound@)
penDefault :: PenStyle
penDefault
  = PenStyle PenSolid black 1 CapRound JoinRound

-- | A solid pen with a certain color and width.
penColored :: Color -> Int -> PenStyle
penColored color width
  = penDefault{ _penColor = color, _penWidth = width }

-- | A transparent pen.
penTransparent :: PenStyle
penTransparent
  = penDefault{ _penKind = PenTransparent }

-- | Dash style
data DashStyle
  = DashDot
  | DashLong
  | DashShort
  | DashDotShort
  --  DashUser [Int]
  deriving (Eq,Show)

-- | Cap style
data CapStyle
  = CapRound          -- ^ End points are rounded
  | CapProjecting
  | CapButt
  deriving (Eq,Show)

-- | Join style.
data JoinStyle
  = JoinRound         -- ^ Corners are rounded
  | JoinBevel         -- ^ Corners are bevelled
  | JoinMiter         -- ^ Corners are blocked
  deriving (Eq,Show)

-- | Hatch style.
data HatchStyle
  = HatchBDiagonal    -- ^ Backward diagonal
  | HatchCrossDiag    -- ^ Crossed diagonal
  | HatchFDiagonal    -- ^ Forward diagonal
  | HatchCross        -- ^ Crossed orthogonal
  | HatchHorizontal   -- ^ Horizontal
  | HatchVertical     -- ^ Vertical
  deriving (Eq,Show)

-- | Brush style.
data BrushStyle
  = BrushStyle { _brushKind :: !BrushKind, _brushColor :: !Color }
  deriving (Eq,Show)

-- | Brush kind.
data BrushKind
  = BrushTransparent                            -- ^ No filling
  | BrushSolid                                  -- ^ Solid color
  | BrushHatch  { _brushHatch  :: !HatchStyle }  -- ^ Hatch pattern
  | BrushStipple{ _brushBitmap :: !(Bitmap ())}  -- ^ Bitmap pattern (on win95 only 8x8 bitmaps are supported)
  deriving (Eq,Show)


-- | Set a pen that is automatically deleted at the end of the computation.
dcWithPenStyle :: DC a -> PenStyle -> IO b -> IO b
dcWithPenStyle dc penStyle io
  = withPenStyle penStyle $ \pen ->
    dcWithPen dc pen io

-- | Set a pen that is used during a certain computation.
dcWithPen :: DC a -> Pen p -> IO b -> IO b
dcWithPen dc pen io
  = bracket  (do oldPen <- dcGetPen dc
                 dcSetPen dc pen
                 return oldPen)
             (\oldPen ->
              do dcSetPen dc oldPen   -- restore previous pen
                 penDelete oldPen)
             (const io)


-- | Set the current pen style. The text color is also adapted.
dcSetPenStyle :: DC a -> PenStyle -> IO ()
dcSetPenStyle dc penStyle
  = withPenStyle penStyle (dcSetPen dc)

-- | Get the current pen style.
dcGetPenStyle :: DC a -> IO PenStyle
dcGetPenStyle dc
  = do pen <- dcGetPen dc
       finalize (penDelete pen) $ 
         do penGetPenStyle pen

-- | Use a pen that is automatically deleted at the end of the computation.
withPenStyle :: PenStyle -> (Pen () -> IO a) -> IO a
withPenStyle penStyle f
  = do (pen,delete) <- penCreateFromStyle penStyle
       finally (f pen) delete

-- | Create a new pen from a 'PenStyle'. Returns both the pen and its deletion procedure.
penCreateFromStyle :: PenStyle -> IO (Pen (),IO ())
penCreateFromStyle penStyle
  = case penStyle of
      PenStyle PenTransparent _color _width _cap _join
        -> do pen <- penCreateFromStock 5 {- transparent -}
              return (pen,return ())
              
      PenStyle (PenDash DashShort) color 1 CapRound JoinRound  | color == black
        -> do pen <- penCreateFromStock 6 {- black dashed -}
              return (pen,return ())
              
      PenStyle PenSolid color 1 CapRound JoinRound
        -> case lookup color stockPens of
             Just idx -> do pen <- penCreateFromStock idx
                            return (pen,return ())
             Nothing  -> colorPen color 1 wxPENSTYLE_SOLID
             
      PenStyle PenSolid color width _cap _join
        -> colorPen color width wxPENSTYLE_SOLID
        
      PenStyle (PenDash dash) color width _cap _join
        -> case dash of
             DashDot      -> colorPen color width wxPENSTYLE_DOT
             DashLong     -> colorPen color width wxPENSTYLE_LONG_DASH
             DashShort    -> colorPen color width wxPENSTYLE_SHORT_DASH
             DashDotShort -> colorPen color width wxPENSTYLE_DOT_DASH
             
      PenStyle (PenStipple bitmap) _color width _cap _join
        -> do pen <- penCreateFromBitmap bitmap width
              setCap pen
              setJoin pen
              return (pen,penDelete pen)
              
      PenStyle (PenHatch hatch) color width _cap _join
        -> case hatch of
             HatchBDiagonal  -> colorPen color width wxPENSTYLE_BDIAGONAL_HATCH
             HatchCrossDiag  -> colorPen color width wxPENSTYLE_CROSSDIAG_HATCH
             HatchFDiagonal  -> colorPen color width wxPENSTYLE_FDIAGONAL_HATCH
             HatchCross      -> colorPen color width wxPENSTYLE_CROSS_HATCH
             HatchHorizontal -> colorPen color width wxPENSTYLE_HORIZONTAL_HATCH
             HatchVertical   -> colorPen color width wxPENSTYLE_VERTICAL_HATCH

  where
    colorPen color width style
      = do pen <- penCreateFromColour color width style
           setCap pen
           setJoin pen
           return (pen,penDelete pen)

    setCap pen
      = case _penCap penStyle of
          CapRound      -> return ()
          CapProjecting -> penSetCap pen wxCAP_PROJECTING
          CapButt       -> penSetCap pen wxCAP_BUTT

    setJoin pen
      = case _penJoin penStyle of
          JoinRound     -> return ()
          JoinBevel     -> penSetJoin pen wxJOIN_BEVEL
          JoinMiter     -> penSetJoin pen wxJOIN_MITER

    stockPens
      = [(red,0),(cyan,1),(green,2)
        ,(black,3),(white,4)
        ,(grey,7),(lightgrey,9)
        ,(mediumgrey,8)
        ]

-- | Create a 'PenStyle' from a 'Pen'.
penGetPenStyle :: Pen a -> IO PenStyle
penGetPenStyle pen
  = if (objectIsNull pen)
     then return penDefault
     else do ok <- penIsOk pen
             if not ok 
              then return penDefault
              else do stl <- penGetStyle pen
                      toPenStyle stl
  where
    toPenStyle stl
      | stl == wxPENSTYLE_TRANSPARENT      = return penTransparent
      | stl == wxPENSTYLE_SOLID            = toPenStyleWithKind PenSolid
      | stl == wxPENSTYLE_DOT              = toPenStyleWithKind (PenDash DashDot)
      | stl == wxPENSTYLE_LONG_DASH        = toPenStyleWithKind (PenDash DashLong)
      | stl == wxPENSTYLE_SHORT_DASH       = toPenStyleWithKind (PenDash DashShort)
      | stl == wxPENSTYLE_DOT_DASH         = toPenStyleWithKind (PenDash DashDotShort)
      | stl == wxPENSTYLE_STIPPLE          = do stipple <- penGetStipple pen
                                                toPenStyleWithKind (PenStipple stipple)
      | stl == wxPENSTYLE_BDIAGONAL_HATCH  = toPenStyleWithKind (PenHatch HatchBDiagonal)
      | stl == wxPENSTYLE_CROSSDIAG_HATCH  = toPenStyleWithKind (PenHatch HatchCrossDiag)
      | stl == wxPENSTYLE_FDIAGONAL_HATCH  = toPenStyleWithKind (PenHatch HatchFDiagonal)
      | stl == wxPENSTYLE_CROSS_HATCH      = toPenStyleWithKind (PenHatch HatchCross)
      | stl == wxPENSTYLE_HORIZONTAL_HATCH = toPenStyleWithKind (PenHatch HatchHorizontal)
      | stl == wxPENSTYLE_VERTICAL_HATCH   = toPenStyleWithKind (PenHatch HatchVertical)
      | otherwise                          = toPenStyleWithKind PenSolid
      
    toPenStyleWithKind kind
      = do width  <- penGetWidth pen
           color  <- penGetColour pen
           cap    <- penGetCap pen
           join   <- penGetJoin pen
           return (PenStyle kind color width (toCap cap) (toJoin join))

    toCap cap
      | cap == wxCAP_PROJECTING = CapProjecting
      | cap == wxCAP_BUTT       = CapButt
      | otherwise               = CapRound

    toJoin join
      | join == wxJOIN_MITER    = JoinMiter
      | join == wxJOIN_BEVEL    = JoinBevel
      | otherwise               = JoinRound


           
{--------------------------------------------------------------------------------
  Brush
--------------------------------------------------------------------------------}
-- | Default brush (transparent, black).
brushDefault :: BrushStyle
brushDefault
  = BrushStyle BrushTransparent black


-- | A solid brush of a specific color.
brushSolid :: Color -> BrushStyle
brushSolid color
  = BrushStyle BrushSolid color

-- | A transparent brush.
brushTransparent :: BrushStyle
brushTransparent
  = BrushStyle BrushTransparent white
-- | Use a brush that is automatically deleted at the end of the computation.
dcWithBrushStyle :: DC a -> BrushStyle -> IO b -> IO b
dcWithBrushStyle dc brushStyle io
  = withBrushStyle brushStyle $ \brush ->
    dcWithBrush dc brush io

dcWithBrush :: DC b -> Brush a -> IO c -> IO c
dcWithBrush dc brush io
  = bracket  (do oldBrush <- dcGetBrush dc
                 dcSetBrush dc brush
                 return oldBrush)
             (\oldBrush ->
              do dcSetBrush dc oldBrush -- restore previous brush
                 brushDelete oldBrush)
             (const io)

-- | Set the brush style (and text background color) of a device context.
dcSetBrushStyle :: DC a -> BrushStyle -> IO ()
dcSetBrushStyle dc brushStyle
  = withBrushStyle brushStyle (dcSetBrush dc)
       
-- | Get the current brush of a device context.
dcGetBrushStyle :: DC a -> IO BrushStyle
dcGetBrushStyle dc
  = do brush <- dcGetBrush dc
       finalize (brushDelete brush) $
        do brushGetBrushStyle brush


-- | Use a brush that is automatically deleted at the end of the computation.
withBrushStyle :: BrushStyle -> (Brush () -> IO a) -> IO a
withBrushStyle brushStyle f
  = do (brush,delete) <- brushCreateFromStyle brushStyle
       finalize delete $ 
        do f brush 

-- | Create a new brush from a 'BrushStyle'. Returns both the brush and its deletion procedure.
brushCreateFromStyle :: BrushStyle -> IO (Brush (), IO ())
brushCreateFromStyle brushStyle
  = case brushStyle of
      BrushStyle BrushTransparent color
        -> do brush <- if (wxToolkit == WxMac)
                        then brushCreateFromColour color wxBRUSHSTYLE_TRANSPARENT
                        else brushCreateFromStock 7   {- transparent brush -}
              return (brush,return ())
      BrushStyle BrushSolid color
        -> case lookup color stockBrushes of
             Just idx  -> do brush <- brushCreateFromStock idx
                             return (brush,return ())
             Nothing   -> colorBrush color wxBRUSHSTYLE_SOLID

      BrushStyle (BrushHatch HatchBDiagonal) color   -> colorBrush color wxBRUSHSTYLE_BDIAGONAL_HATCH
      BrushStyle (BrushHatch HatchCrossDiag) color   -> colorBrush color wxBRUSHSTYLE_CROSSDIAG_HATCH
      BrushStyle (BrushHatch HatchFDiagonal) color   -> colorBrush color wxBRUSHSTYLE_FDIAGONAL_HATCH
      BrushStyle (BrushHatch HatchCross) color       -> colorBrush color wxBRUSHSTYLE_CROSS_HATCH
      BrushStyle (BrushHatch HatchHorizontal) color  -> colorBrush color wxBRUSHSTYLE_HORIZONTAL_HATCH
      BrushStyle (BrushHatch HatchVertical) color    -> colorBrush color wxBRUSHSTYLE_VERTICAL_HATCH
      BrushStyle (BrushStipple bitmap)      _color   -> do brush <- brushCreateFromBitmap bitmap
                                                           return (brush, brushDelete brush)
  where
    colorBrush color style
      = do brush <- brushCreateFromColour color style
           return (brush, brushDelete brush )

    stockBrushes
      = [(blue,0),(green,1),(white,2)
        ,(black,3),(grey,4),(lightgrey,6)
        ,(cyan,8),(red,9)
        ,(mediumgrey,5)
        ]

-- | Get the 'BrushStyle' of 'Brush'.
brushGetBrushStyle :: Brush a -> IO BrushStyle
brushGetBrushStyle brush
  = if (objectIsNull brush)
     then return brushDefault
     else do ok <- brushIsOk brush
             if not ok
              then return brushDefault
              else do stl   <- brushGetStyle brush
                      kind  <- toBrushKind stl
                      color <- brushGetColour brush
                      return (BrushStyle kind color)
  where
    toBrushKind stl
      | stl == wxBRUSHSTYLE_TRANSPARENT      = return BrushTransparent
      | stl == wxBRUSHSTYLE_SOLID            = return BrushSolid
      | stl == wxBRUSHSTYLE_STIPPLE          = do stipple <- brushGetStipple brush
                                                  return (BrushStipple stipple)
      | stl == wxBRUSHSTYLE_BDIAGONAL_HATCH  = return (BrushHatch HatchBDiagonal)
      | stl == wxBRUSHSTYLE_CROSSDIAG_HATCH  = return (BrushHatch HatchCrossDiag)
      | stl == wxBRUSHSTYLE_FDIAGONAL_HATCH  = return (BrushHatch HatchFDiagonal)
      | stl == wxBRUSHSTYLE_CROSS_HATCH      = return (BrushHatch HatchCross)
      | stl == wxBRUSHSTYLE_HORIZONTAL_HATCH = return (BrushHatch HatchHorizontal)
      | stl == wxBRUSHSTYLE_VERTICAL_HATCH   = return (BrushHatch HatchVertical)
      | otherwise                            = return BrushTransparent



{--------------------------------------------------------------------------------
  DC drawing state
--------------------------------------------------------------------------------}
-- | The drawing state (pen,brush,font,text color,text background color) of a device context.
data DrawState  = DrawState (Pen ()) (Brush ()) (Font ()) Color Color

-- | Run a computation after which the original drawing state of the 'DC' is restored.
dcEncapsulate :: DC a -> IO b -> IO b
dcEncapsulate dc io
  = bracket (dcGetDrawState dc)
            (\drawState ->
             do dcSetDrawState dc drawState
                drawStateDelete drawState)
            (const io)

-- | Get the drawing state. (Should be deleted with 'drawStateDelete').
dcGetDrawState     :: DC a -> IO DrawState
dcGetDrawState dc
  = do pen   <- dcGetPen dc
       brush <- dcGetBrush dc
       font  <- dcGetFont dc
       textc <- dcGetTextForeground dc
       backc <- dcGetTextBackground dc
       return (DrawState pen brush font textc backc)

-- | Set the drawing state.
dcSetDrawState  :: DC a -> DrawState -> IO ()
dcSetDrawState dc (DrawState pen brush font textc backc)
  = do dcSetPen dc pen
       dcSetBrush dc brush
       dcSetFont dc font
       dcSetTextBackground dc backc
       dcSetTextForeground dc textc

-- | Release the resources associated with a drawing state.
drawStateDelete :: DrawState -> IO ()
drawStateDelete (DrawState pen brush font _ _)
  = do penDelete pen
       brushDelete brush
       fontDelete font

{--------------------------------------------------------------------------------
  DC utils
--------------------------------------------------------------------------------}

-- | Draw connected lines.
drawLines :: DC a -> [Point] -> IO ()
drawLines _dc [] = return ()
drawLines dc  ps
  = withArray xs $ \pxs ->
    withArray ys $ \pys ->
    dcDrawLines dc n pxs pys (pt 0 0)
  where
    n  = length ps
    xs = map pointX ps
    ys = map pointY ps


-- | Draw a polygon. The polygon is filled with the odd-even rule.
drawPolygon :: DC a -> [Point] -> IO ()
drawPolygon _dc [] = return ()
drawPolygon dc  ps
  = withArray xs $ \pxs ->
    withArray ys $ \pys ->
    dcDrawPolygon dc n pxs pys (pt 0 0) wxODDEVEN_RULE
  where
    n  = length ps
    xs = map pointX ps
    ys = map pointY ps

-- | Gets the dimensions of the string using the currently selected font.
getTextExtent :: DC a -> String -> IO Size
getTextExtent dc txt
  = do (sz',_,_) <- getFullTextExtent dc txt
       return sz'

-- | Gets the dimensions of the string using the currently selected font.
-- Takes text string to measure, and returns the size, /descent/ and /external leading/.
-- Descent is the dimension from the baseline of the font to the bottom of the descender
-- , and external leading is any extra vertical space added to the font by the font designer (is usually zero).
getFullTextExtent :: DC a -> String -> IO (Size,Int,Int)
getFullTextExtent dc txt
  = alloca $ \px ->
    alloca $ \py ->
    alloca $ \pd ->
    alloca $ \pe ->
    do dcGetTextExtent dc txt px py pd pe objectNull
       x <- peek px
       y <- peek py
       d <- peek pd
       e <- peek pe
       return (sz (fromCInt x) (fromCInt y), fromCInt d, fromCInt e)


{--------------------------------------------------------------------------------
  Double buffering
--------------------------------------------------------------------------------}

-- | Use double buffering to draw to a 'DC' -- reduces flicker. Note that
-- the 'windowOnPaint' handler can already take care of buffering automatically.
-- The rectangle argument is normally the view rectangle ('windowGetViewRect').
-- Uses a 'MemoryDC' to draw into memory first and than blit the result to
-- the device context. The memory area allocated is the minimal size necessary
-- to accomodate the rectangle, but is re-allocated on each invokation.
dcBuffer :: WindowDC a -> Rect -> (DC () -> IO ()) -> IO ()
dcBuffer dc r draw
  = dcBufferWithRef dc Nothing r draw

-- | Optimized double buffering. Takes a possible reference to a bitmap. If it is
-- 'Nothing', a new bitmap is allocated everytime. Otherwise, the reference is used
-- to re-use an allocated bitmap if possible. The 'Rect' argument specifies the
-- the current logical view rectangle. The last argument is called to draw on the
-- memory 'DC'. 
dcBufferWithRef :: WindowDC a -> Maybe (Var (Bitmap ())) -> Rect -> (DC () -> IO ()) -> IO ()
dcBufferWithRef dc mbVar viewArea draw
  = dcBufferWithRefEx dc (\dc' -> dcClearRect dc' viewArea) mbVar viewArea draw


-- | Optimized double buffering. Takes a /clear/ routine as its first argument.
-- Normally this is something like '\dc -> dcClearRect dc viewArea' but on certain platforms, like
-- MacOS X, special handling is necessary.
dcBufferWithRefEx :: WindowDC a -> (DC () -> IO ()) -> Maybe (Var (Bitmap ()))
                  -> Rect -> (DC () -> IO ()) -> IO ()
dcBufferWithRefEx = dcBufferedAux simpleDraw simpleDraw
  where simpleDraw dc draw = draw $ downcastDC dc

-- | Optimized double buffering with a GCDC. Takes a /clear/ routine as its first argument.
-- Normally this is something like '\dc -> dcClearRect dc viewArea' but on certain platforms, like
-- MacOS X, special handling is necessary.
dcBufferWithRefExGcdc :: WindowDC a -> (DC () -> IO ()) -> Maybe (Var (Bitmap ()))
                      -> Rect -> (GCDC () -> IO b) -> IO ()
dcBufferWithRefExGcdc =
  dcBufferedAux (withGC gcdcCreate) (withGC gcdcCreateFromMemory)
    where withGC create dc_ draw = do
            dc <- create dc_
            _  <- draw dc
            gcdcDelete dc

dcBufferedAux :: (WindowDC a -> f -> IO ()) -> (MemoryDC c -> f -> IO ())
              -> WindowDC a -> (DC () -> IO ()) -> Maybe (Var (Bitmap ()))
              -> Rect -> f -> IO ()
dcBufferedAux _ _  _ _ _ view _
  | rectSize view == sizeZero = return ()
dcBufferedAux withWinDC withMemoryDC dc clear mbVar view draw
  = bracket (initBitmap)
            (doneBitmap)
            (\bitmap ->
             if (bitmap==objectNull)
              then drawUnbuffered
              else bracket (do p <- memoryDCCreateCompatible dc; return (objectCast p))
                           (\memdc -> when (memdc/=objectNull) (memoryDCDelete memdc))
                           (\memdc -> if (memdc==objectNull)
                                      then drawUnbuffered
                                      else do memoryDCSelectObject memdc bitmap
                                              drawBuffered memdc
                                              memoryDCSelectObject memdc nullBitmap))
    where
     initBitmap
       = case mbVar of
           Nothing  -> bitmapCreateEmpty (rectSize view) (-1)
           Just v   -> do bitmap <- varGet v
                          size   <- if (bitmap==objectNull)
                                    then return sizeZero
                                    else do bw <- bitmapGetWidth bitmap
                                            bh <- bitmapGetHeight bitmap
                                            return (Size bw bh)
                          -- re-use the bitmap if possible
                          if (sizeEncloses size (rectSize view) && bitmap /= objectNull)
                            then return bitmap
                            else do when (bitmap/=objectNull) (bitmapDelete bitmap)
                                    varSet v objectNull
                                    -- new size a bit larger to avoid multiple reallocs
                                    let (Size w h) = rectSize view
                                        neww       = div (w*105) 100
                                        newh       = div (h*105) 100
                                    if (w > 0 && h > 0) then
                                      do bm <- bitmapCreateEmpty (sz neww newh) (-1)
                                         varSet v bm
                                         return bm
                                      else return objectNull

     doneBitmap bitmap
       = case mbVar of
           Nothing -> when (bitmap/=objectNull) (bitmapDelete bitmap)
           Just _v -> return ()


     drawUnbuffered
       = do clear (downcastDC dc)
            withWinDC dc draw

     drawBuffered memdc
      = do -- set the device origin for scrolled windows
           dcSetDeviceOrigin memdc (pointFromVec (vecNegate (vecFromPoint (rectTopLeft view))))
           dcSetClippingRegion memdc view
           -- dcBlit memdc view dc (rectTopLeft view) wxCOPY False
           bracket (dcGetBackground dc)
                   (\brush -> do dcSetBrush memdc nullBrush
                                 brushDelete brush)
                   (\brush -> do -- set the background to the owner brush
                                 dcSetBackground memdc brush
                                 if (wxToolkit == WxMac)
                                  then withBrushStyle brushTransparent (dcSetBrush memdc)
                                  else dcSetBrush memdc brush
                                 clear (downcastDC memdc)
                                 -- and finally do the drawing!
                                 withMemoryDC memdc draw
                   )
           -- blit the memdc into the owner dc.
           _ <- dcBlit dc view memdc (rectTopLeft view) wxCOPY False
           return ()

