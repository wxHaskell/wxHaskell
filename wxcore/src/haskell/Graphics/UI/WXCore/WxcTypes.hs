{-# LANGUAGE CPP, ForeignFunctionInterface, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------------------
{-|
Module      :  WxcTypes
Copyright   :  (c) Daan Leijen 2003, 2004
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Basic types and marshalling code for the wxWidgets C library.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.WxcTypes(
            -- * Object types
              Object, objectNull, objectIsNull, objectCast, objectIsManaged
            , objectDelete
            , objectFromPtr, managedObjectFromPtr
            , withObjectPtr, withObjectRef
            , withObjectResult, withManagedObjectResult
            , objectFinalize, objectNoFinalize
            
--            , Managed, managedNull, managedIsNull, managedCast, createManaged, withManaged, managedTouch

            -- * Type synonyms
            , Id
            , Style
            , EventId

            -- * Basic types
            , fromBool, toBool

            -- ** Point
            , Point, Point2(Point,pointX,pointY), point, pt, pointFromVec, pointFromSize, pointZero, pointNull

            -- ** Size
            , Size, Size2D(Size,sizeW,sizeH), sz, sizeFromPoint, sizeFromVec, sizeZero, sizeNull

            -- ** Vector
            , Vector, Vector2(Vector,vecX,vecY), vector, vec, vecFromPoint, vecFromSize, vecZero, vecNull

            -- ** Rectangle
            , Rect, Rect2D(Rect,rectLeft,rectTop,rectWidth,rectHeight)
            , rectTopLeft, rectTopRight, rectBottomLeft, rectBottomRight, rectBottom, rectRight
            , rect, rectBetween, rectFromSize, rectZero, rectNull, rectSize, rectIsEmpty

            -- ** Color
            , Color(..), rgb, colorRGB, rgba, colorRGBA, colorRed, colorGreen, colorBlue, colorAlpha
            , intFromColor, colorFromInt, fromColor, toColor, colorOk, colorIsOk

            -- * Marshalling
            -- ** Basic types
            , withPointResult, withWxPointResult, toCIntPointX, toCIntPointY, fromCPoint, withCPoint
            , withPointDoubleResult, toCDoublePointX, toCDoublePointY, fromCPointDouble, withCPointDouble
            , withSizeResult, withWxSizeResult, toCIntSizeW, toCIntSizeH, fromCSize, withCSize
            , withSizeDoubleResult, toCDoubleSizeW, toCDoubleSizeH, fromCSizeDouble, withCSizeDouble
            , withVectorResult, withWxVectorResult, toCIntVectorX, toCIntVectorY, fromCVector, withCVector
            , withVectorDoubleResult, toCDoubleVectorX, toCDoubleVectorY, fromCVectorDouble, withCVectorDouble
            , withRectResult, withWxRectResult, withWxRectPtr, toCIntRectX, toCIntRectY, toCIntRectW, toCIntRectH, fromCRect, withCRect
            , withRectDoubleResult, toCDoubleRectX, toCDoubleRectY, toCDoubleRectW, toCDoubleRectH, fromCRectDouble, withCRectDouble
            , withArray, withArrayString, withArrayWString, withArrayInt, withArrayIntPtr, withArrayObject
            , withArrayIntResult, withArrayIntPtrResult, withArrayStringResult, withArrayWStringResult, withArrayObjectResult

            , colourFromColor, colorFromColour
            , colourCreate, colourSafeDelete -- , colourCreateRGB, colourRed, colourGreen, colourBlue colourAlpha

  
            -- ** Managed object types

            -- , managedAddFinalizer
            , TreeItem, treeItemInvalid, treeItemIsOk, treeItemFromInt
            , withRefTreeItemId, withTreeItemIdPtr, withTreeItemIdRef, withManagedTreeItemIdResult
            , withStringRef, withStringPtr, withManagedStringResult
            , withRefColour, withColourRef, withColourPtr, withManagedColourResult
            , withRefBitmap, withManagedBitmapResult
            , withRefCursor, withManagedCursorResult
            , withRefIcon, withManagedIconResult
            , withRefPen, withManagedPenResult
            , withRefBrush, withManagedBrushResult
            , withRefFont, withManagedFontResult
            , withRefImage
            , withRefListItem
            , withRefFontData
            , withRefPrintData
            , withRefPageSetupDialogData
            , withRefPrintDialogData
            , withRefDateTime, withManagedDateTimeResult
            , withRefGridCellCoordsArray, withManagedGridCellCoordsArrayResult

            -- ** Primitive types
            -- *** CString
            , CString, withCString, withStringResult
            , CWString, withCWString, withWStringResult
            -- *** ByteString
            , withByteStringResult, withLazyByteStringResult
            -- *** CInt
            , CInt(..), toCInt, fromCInt, withIntResult
            -- *** IntPtr
            , IntPtr
            -- *** CIntPtr
            , CIntPtr, toCIntPtr, fromCIntPtr, withIntPtrResult
            -- *** Word
            , Word
            -- *** 8 bit Word
            , Word8
            -- *** 64 bit Integer
            , Int64
            -- *** CDouble
            , CDouble(..), toCDouble, fromCDouble, withDoubleResult
            -- *** CChar
            , CChar, toCChar, fromCChar, withCharResult
            , CWchar(..), toCWchar
            -- *** CBool
            , CBool(..), toCBool, fromCBool, withBoolResult
            -- ** Pointers
            , Ptr, ptrNull, ptrIsNull, ptrCast, ForeignPtr, FunPtr, toCFunPtr
            ) where

#include "wxc/wxc_def.h"

import Control.Exception 
import Data.Ix
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (fromBool, toBool)

{- note: for GHC 6.10.2 or higher, recommends to use "import Foreign.Concurrent"
   See http://www.haskell.org/pipermail/cvs-ghc/2009-January/047120.html -}
import Foreign.ForeignPtr hiding (newForeignPtr,addForeignPtrFinalizer)

import Data.Bits( shiftL, shiftR, (.&.), (.|.) )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Data.ByteString.Lazy.Char8 as LBC (pack)

{- note: this is just for instances for the WX library and not necessary for WXCore -}
import Data.Dynamic

import Data.Int
import Data.Word
import Debug.Trace (traceIO)

import Graphics.UI.WXCore.WxcObject
import Graphics.UI.WXCore.WxcClassTypes

{-----------------------------------------------------------------------------------------
    Objects
-----------------------------------------------------------------------------------------}
-- | An @Id@ is used to identify objects during event handling.
type Id = Int

-- | An @EventId@ is identifies specific events.
type EventId = Int

-- | A @Style@ is normally used as a flag mask to specify some window style
type Style = Int


-- | Delete a wxObject, works for managed and unmanaged objects.
objectDelete :: WxObject a -> IO ()
objectDelete obj
  = if objectIsManaged obj
     then objectFinalize obj
     else withObjectPtr obj $ \p ->
          wxObject_SafeDelete p
                                    
-- | Create a managed object that will be deleted using |wxObject_SafeDelete|.
managedObjectFromPtr :: Ptr (TWxObject a) -> IO (WxObject a)
managedObjectFromPtr p 
  = do mp <- wxManagedPtr_CreateFromObject p
       objectFromManagedPtr mp

-- | Create a managed object that will be deleted using |wxObject_SafeDelete|.
withManagedObjectResult :: IO (Ptr (TWxObject a)) -> IO (WxObject a)
withManagedObjectResult io
  = do p <- io
       managedObjectFromPtr p

-- | Return an unmanaged object.
withObjectResult :: IO (Ptr a) -> IO (Object a)
withObjectResult io
  = do p <- io
       return (objectFromPtr p)

-- | Extract the object pointer and raise an exception if @NULL@.
-- Otherwise continue with the valid pointer.
withObjectRef :: String -> Object a -> (Ptr a -> IO b) -> IO b
withObjectRef msg obj f
  = withObjectPtr obj $ \p -> 
    withValidPtr msg p f

-- | Execute the continuation when the pointer is not null and
-- raise an error otherwise.
withValidPtr :: String -> Ptr a -> (Ptr a -> IO b) -> IO b
withValidPtr msg p f
  = if (p == nullPtr)
     then ioError (userError ("wxHaskell: NULL object" ++ (if null msg then "" else ": " ++ msg)))
     else f p


foreign import ccall wxManagedPtr_CreateFromObject :: Ptr (TWxObject a) -> IO (ManagedPtr (TWxObject a))
foreign import ccall wxObject_SafeDelete           :: Ptr (TWxObject a) -> IO ()


{-----------------------------------------------------------------------------------------
  Point
-----------------------------------------------------------------------------------------}
--- | Define Point type synonym for backward compatibility.
type Point = Point2 Int

-- | A point has an x and y coordinate. Coordinates are normally relative to the
-- upper-left corner of their view frame, where a positive x goes to the right and
-- a positive y to the bottom of the view.
data (Num a) => Point2 a = Point
        { pointX :: !a -- ^ x component of a point.
        , pointY :: !a -- ^ y component of a point.
        }
        deriving (Eq,Show,Read,Typeable)

-- Moved from Types.hs at 2015-09-01
instance (Num a, Ord a) => Ord (Point2 a) where
  compare (Point x1 y1) (Point x2 y2)             
    = case compare y1 y2 of
        EQ  -> compare x1 x2
        neq -> neq

-- Moved from Types.hs at 2015-09-01
instance Ix (Point2 Int) where
  range (Point x1 y1,Point x2 y2)             
    = [Point x y | y <- [y1..y2], x <- [x1..x2]]

  inRange (Point x1 y1, Point x2 y2) (Point x y)
    = (x >= x1 && x <= x2 && y >= y1 && y <= y2)

  rangeSize (Point x1 y1, Point x2 y2) 
    = let w = abs (x2 - x1) + 1
          h = abs (y2 - y1) + 1
      in w*h

  index bnd@(Point x1 y1, Point x2 _y2) p@(Point x y)
    = if inRange bnd p
       then let w = abs (x2 - x1) + 1
            in (y-y1)*w + x
       else error ("Point index out of bounds: " ++ show p ++ " not in " ++ show bnd)


-- | Construct a point.
point :: (Num a) => a -> a -> Point2 a
point x y  = Point x y

-- | Shorter function to construct a point.
pt :: (Num a) => a -> a -> Point2 a
pt x y  = Point x y

pointFromVec :: (Num a) => Vector -> Point2 a
pointFromVec (Vector x y)
  = Point (fromIntegral x) (fromIntegral y)

pointFromSize :: (Num a) => Size -> Point2 a
pointFromSize (Size w h)
  = Point (fromIntegral w) (fromIntegral h)

-- | Point at the origin.
pointZero :: (Num a) => Point2 a
pointZero
  = Point 0 0

-- | A `null' point is not a legal point (x and y are -1) and can be used for some
-- wxWidgets functions to select a default point.
pointNull :: (Num a) => Point2 a
pointNull
  = Point (-1) (-1)

-- marshalling
withCPoint :: Point2 Int -> (CInt -> CInt -> IO a) -> IO a
withCPoint (Point x y) f
  = f (toCInt x) (toCInt y)

withPointResult :: (Ptr CInt -> Ptr CInt -> IO ()) -> IO (Point2 Int)
withPointResult f
  = alloca $ \px ->
    alloca $ \py ->
    do f px py
       x <- peek px
       y <- peek py
       return (fromCPoint x y)

toCIntPointX, toCIntPointY :: Point2 Int -> CInt
toCIntPointX (Point x _y)  = toCInt x
toCIntPointY (Point _x y)  = toCInt y

fromCPoint :: CInt -> CInt -> Point2 Int
fromCPoint x y
  = Point (fromCInt x) (fromCInt y)

withCPointDouble :: Point2 Double -> (CDouble -> CDouble -> IO a) -> IO a
withCPointDouble (Point x y) f
  = f (toCDouble x) (toCDouble y)

withPointDoubleResult :: (Ptr CDouble -> Ptr CDouble -> IO ()) -> IO (Point2 Double)
withPointDoubleResult f
  = alloca $ \px ->
    alloca $ \py ->
    do f px py
       x <- peek px
       y <- peek py
       return (fromCPointDouble x y)

toCDoublePointX, toCDoublePointY :: Point2 Double -> CDouble
toCDoublePointX (Point x _y) = toCDouble x
toCDoublePointY (Point _x y) = toCDouble y

fromCPointDouble :: CDouble -> CDouble -> Point2 Double
fromCPointDouble x y
  = Point (fromCDouble x) (fromCDouble y)

{-
-- | A @wxPoint@ object.
type WxPointObject a   = Ptr (CWxPointObject a)
type TWxPointObject a  = CWxPointObject a
data CWxPointObject a  = CWxPointObject
-}

withWxPointResult :: IO (Ptr (TWxPoint a)) -> IO (Point2 Int)
withWxPointResult io
  = do poynt <- io
       x  <- wxPoint_GetX poynt
       y  <- wxPoint_GetY poynt
       wxPoint_Delete poynt
       return (fromCPoint x y)

foreign import ccall wxPoint_Delete :: Ptr (TWxPoint a) -> IO ()
foreign import ccall wxPoint_GetX   :: Ptr (TWxPoint a) -> IO CInt
foreign import ccall wxPoint_GetY   :: Ptr (TWxPoint a) -> IO CInt


{-----------------------------------------------------------------------------------------
  Size
-----------------------------------------------------------------------------------------}
--- | Define Point type synonym for backward compatibility.
type Size = Size2D Int

-- | A @Size@ has a width and height.
data (Num a) => Size2D a = Size
        { sizeW :: !a -- ^ the width  of a size
        , sizeH :: !a -- ^ the height of a size
        }
        deriving (Eq,Show,Typeable)

{-
-- | Construct a size from a width and height.
size :: (Num a) => a -> a -> Size2D a
size w h
  = Size w h
-}

-- | Short function to construct a size
sz :: (Num a) => a -> a -> Size2D a
sz w h
  = Size w h

sizeFromPoint :: (Num a) => Point2 a -> Size2D a
sizeFromPoint (Point x y)
  = Size x y

sizeFromVec   :: (Num a) => Vector2 a -> Size2D a
sizeFromVec (Vector x y)
  = Size x y

sizeZero :: (Num a) => Size2D a
sizeZero
  = Size 0 0

-- | A `null' size is not a legal size (width and height are -1) and can be used for some
-- wxWidgets functions to select a default size.
sizeNull :: (Num a) => Size2D a
sizeNull
  = Size (-1) (-1)

-- marshalling
withCSize :: Size -> (CInt -> CInt -> IO a) -> IO a
withCSize (Size w h) f
  = f (toCInt w) (toCInt h)

withSizeResult :: (Ptr CInt -> Ptr CInt -> IO ()) -> IO Size
withSizeResult f
  = alloca $ \cw ->
    alloca $ \ch ->
    do f cw ch
       w <- peek cw
       h <- peek ch
       return (fromCSize w h)

fromCSize :: CInt -> CInt -> Size
fromCSize w h
  = Size (fromCInt w) (fromCInt h)

toCIntSizeW, toCIntSizeH :: Size -> CInt
toCIntSizeW (Size w _h) = toCInt w
toCIntSizeH (Size _w h) = toCInt h

withCSizeDouble :: Size2D Double -> (CDouble -> CDouble -> IO a) -> IO a
withCSizeDouble (Size w h) f
  = f (toCDouble w) (toCDouble h)

withSizeDoubleResult :: (Ptr CDouble -> Ptr CDouble -> IO ()) -> IO (Size2D Double)
withSizeDoubleResult f
  = alloca $ \cw ->
    alloca $ \ch ->
    do f cw ch
       w <- peek cw
       h <- peek ch
       return (fromCSizeDouble w h)

fromCSizeDouble :: CDouble -> CDouble -> Size2D Double
fromCSizeDouble w h
  = Size (fromCDouble w) (fromCDouble h)

toCDoubleSizeW, toCDoubleSizeH :: Size2D Double -> CDouble
toCDoubleSizeW (Size w _h) = toCDouble w
toCDoubleSizeH (Size _w h) = toCDouble h

{-
-- | A @wxSize@ object.
type WxSizeObject a   = Ptr (CWxSizeObject a)
type TWxSizeObject a  = CWxSizeObject a
data CWxSizeObject a  = CWxSizeObject
-}

withWxSizeResult :: IO (Ptr (TWxSize a)) -> IO Size
withWxSizeResult io
  = do size <- io
       w  <- wxSize_GetWidth  size
       h  <- wxSize_GetHeight size
       wxSize_Delete size
       return (fromCSize w h)

foreign import ccall wxSize_Delete    :: Ptr (TWxSize a) -> IO ()
foreign import ccall wxSize_GetWidth  :: Ptr (TWxSize a) -> IO CInt
foreign import ccall wxSize_GetHeight :: Ptr (TWxSize a) -> IO CInt


{-----------------------------------------------------------------------------------------
  Vector
-----------------------------------------------------------------------------------------}
--- | Define Point type synonym for backward compatibility.
type Vector = Vector2 Int

-- | A vector with an x and y delta.
data (Num a) => Vector2 a = Vector
        { vecX :: !a -- ^ delta-x component of a vector
        , vecY :: !a -- ^ delta-y component of a vector
        }
        deriving (Eq,Show,Read,Typeable)

-- | Construct a vector.
vector :: (Num a) => a -> a -> Vector2 a
vector dx dy  = Vector dx dy

-- | Short function to construct a vector.
vec :: (Num a) => a -> a -> Vector2 a
vec dx dy  = Vector dx dy

-- | A zero vector
vecZero :: (Num a) => Vector2 a
vecZero
  = Vector 0 0

-- | A `null' vector has a delta x and y of -1 and can be used for some
-- wxWidgets functions to select a default vector.
vecNull :: (Num a) => Vector2 a
vecNull
  = Vector (-1) (-1)

vecFromPoint :: (Num a) => Point2 a -> Vector2 a
vecFromPoint (Point x y)
  = Vector x y

vecFromSize :: Size -> Vector
vecFromSize (Size w h)
  = Vector w h


-- marshalling
withCVector :: Vector -> (CInt -> CInt -> IO a) -> IO a
withCVector (Vector x y) f
  = f (toCInt x) (toCInt y)

withVectorResult :: (Ptr CInt -> Ptr CInt -> IO ()) -> IO Vector
withVectorResult f
  = alloca $ \px ->
    alloca $ \py ->
    do f px py
       x <- peek px
       y <- peek py
       return (fromCVector x y)

toCIntVectorX, toCIntVectorY :: Vector -> CInt
toCIntVectorX (Vector x _y) = toCInt x
toCIntVectorY (Vector _x y) = toCInt y

fromCVector :: CInt -> CInt -> Vector
fromCVector x y
  = Vector (fromCInt x) (fromCInt y)

withCVectorDouble :: Vector2 Double -> (CDouble -> CDouble -> IO a) -> IO a
withCVectorDouble (Vector x y) f
  = f (toCDouble x) (toCDouble y)

withVectorDoubleResult :: (Ptr CDouble -> Ptr CDouble -> IO ()) -> IO (Vector2 Double)
withVectorDoubleResult f
  = alloca $ \px ->
    alloca $ \py ->
    do f px py
       x <- peek px
       y <- peek py
       return (fromCVectorDouble x y)

toCDoubleVectorX, toCDoubleVectorY :: Vector2 Double -> CDouble
toCDoubleVectorX (Vector x _y) = toCDouble x
toCDoubleVectorY (Vector _x y) = toCDouble y

fromCVectorDouble :: CDouble -> CDouble -> Vector2 Double
fromCVectorDouble x y
  = Vector (fromCDouble x) (fromCDouble y)


withWxVectorResult :: IO (Ptr (TWxPoint a)) -> IO Vector
withWxVectorResult io
  = do poynt <- io
       x  <- wxPoint_GetX poynt
       y  <- wxPoint_GetY poynt
       wxPoint_Delete poynt
       return (fromCVector x y)


{-----------------------------------------------------------------------------------------
  Rectangle
-----------------------------------------------------------------------------------------}
--- | Define Point type synonym for backward compatibility.
type Rect = Rect2D Int

-- | A rectangle is defined by the left x coordinate, the top y coordinate,
-- the width and the height.
data (Num a) => Rect2D a = Rect
        { rectLeft   :: !a
        , rectTop    :: !a
        , rectWidth  :: !a
        , rectHeight :: !a
        }
        deriving (Eq,Show,Read,Typeable)


rectTopLeft, rectTopRight, rectBottomLeft, rectBottomRight :: (Num a) => Rect2D a -> Point2 a
rectTopLeft     (Rect l t _w _h) = Point l       t
rectTopRight    (Rect l t w  _h) = Point (l + w) t
rectBottomLeft  (Rect l t _w h)  = Point l       (t + h)
rectBottomRight (Rect l t w  h)  = Point (l + w) (t + h)

rectBottom, rectRight :: (Num a) => Rect2D a -> a
rectBottom (Rect _x y _w h) = y + h
rectRight  (Rect x _y w _h) = x + w

-- | Create a rectangle at a certain (upper-left) point with a certain size.
rect :: (Num a) => Point2 a -> Size2D a -> Rect2D a
rect (Point x y) (Size w h)
  = Rect x y w h

-- | Construct a (positive) rectangle between two (arbitrary) points.
rectBetween :: (Num a, Ord a) => Point2 a -> Point2 a -> Rect2D a
rectBetween (Point x0 y0) (Point x1 y1)
  = Rect (min x0 x1) (min y0 y1) (abs (x1-x0)) (abs (y1-y0))

-- | An empty rectangle at (0,0).
rectZero :: (Num a) => Rect2D a
rectZero
  = Rect 0 0 0 0

-- | An `null' rectangle is not a valid rectangle (@Rect -1 -1 -1 -1@) but can
-- used for some wxWidgets functions to select a default rectangle. (i.e. 'frameCreate').
rectNull :: (Num a) => Rect2D a
rectNull
  = Rect (-1) (-1) (-1) (-1)

-- | Get the size of a rectangle.
rectSize :: (Num a) => Rect2D a -> Size2D a
rectSize (Rect _l _t w h)
  = Size w h

-- | Create a rectangle of a certain size with the upper-left corner at ('pt' 0 0).
rectFromSize :: (Num a) => Size2D a -> Rect2D a
rectFromSize (Size w h)
  = Rect 0 0 w h

rectIsEmpty :: (Num a, Eq a) => Rect2D a -> Bool
rectIsEmpty (Rect _l _t w h)
  = (w==0 && h==0)



-- marshalling 1
withCRect :: Rect -> (CInt -> CInt -> CInt -> CInt -> IO a) -> IO a
withCRect (Rect x0 y0 x1 y1) f
  = f (toCInt (x0)) (toCInt (y0)) (toCInt (x1)) (toCInt (y1))

withRectResult :: (Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()) -> IO Rect
withRectResult f
  = alloca $ \cx ->
    alloca $ \cy ->
    alloca $ \cw ->
    alloca $ \ch ->
    do f cx cy cw ch
       x <- peek cx
       y <- peek cy
       w <- peek cw
       h <- peek ch
       return (fromCRect x y w h)

fromCRect :: CInt -> CInt -> CInt -> CInt -> Rect
fromCRect x y w h
  = Rect (fromCInt x) (fromCInt y) (fromCInt w) (fromCInt h)

toCIntRectX, toCIntRectY, toCIntRectW, toCIntRectH :: Rect -> CInt
toCIntRectX (Rect  x _y _w _h) = toCInt x
toCIntRectY (Rect _x  y _w _h) = toCInt y
toCIntRectW (Rect _x _y  w _h) = toCInt w
toCIntRectH (Rect _x _y _w  h) = toCInt h

withCRectDouble :: Rect2D Double -> (CDouble -> CDouble -> CDouble -> CDouble -> IO a) -> IO a
withCRectDouble (Rect x0 y0 x1 y1) f
  = f (toCDouble (x0)) (toCDouble (y0)) (toCDouble (x1)) (toCDouble (y1))

withRectDoubleResult :: (Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()) -> IO (Rect2D Double)
withRectDoubleResult f
  = alloca $ \cx ->
    alloca $ \cy ->
    alloca $ \cw ->
    alloca $ \ch ->
    do f cx cy cw ch
       x <- peek cx
       y <- peek cy
       w <- peek cw
       h <- peek ch
       return (fromCRectDouble x y w h)

fromCRectDouble :: CDouble -> CDouble -> CDouble -> CDouble -> Rect2D Double
fromCRectDouble x y w h
  = Rect (fromCDouble x) (fromCDouble y) (fromCDouble w) (fromCDouble h)

toCDoubleRectX, toCDoubleRectY, toCDoubleRectW, toCDoubleRectH :: Rect2D Double -> CDouble
toCDoubleRectX (Rect  x _y _w _h) = toCDouble x
toCDoubleRectY (Rect _x  y _w _h) = toCDouble y
toCDoubleRectW (Rect _x _y  w _h) = toCDouble w
toCDoubleRectH (Rect _x _y _w  h) = toCDouble h

-- marshalling 2
{-
-- | A @wxRect@ object.
type WxRectObject a   = Ptr (CWxRectObject a)
type TWxRectObject a  = CWxRectObject a
data CWxRectObject a  = CWxRectObject
-}

{-
withWxRectRef :: String -> Rect -> (Ptr (TWxRect r) -> IO a) -> IO a
withWxRectRef msg r f
  = withWxRectPtr r $ \p -> withValidPtr msg p f
-}

withWxRectPtr :: Rect -> (Ptr (TWxRect r) -> IO a) -> IO a
withWxRectPtr r f
  = bracket (withCRect r wxRect_Create)
            (wxRect_Delete)
            f

withWxRectResult :: IO (Ptr (TWxRect a)) -> IO Rect
withWxRectResult io
  = do rt <- io
       x  <- wxRect_GetX  rt
       y  <- wxRect_GetY  rt
       w  <- wxRect_GetWidth  rt
       h  <- wxRect_GetHeight rt
       wxRect_Delete rt
       return (fromCRect x y w h)

foreign import ccall wxRect_Create    :: CInt -> CInt -> CInt -> CInt -> IO (Ptr (TWxRect a))
foreign import ccall wxRect_Delete    :: Ptr (TWxRect a) -> IO ()
foreign import ccall wxRect_GetX      :: Ptr (TWxRect a) -> IO CInt
foreign import ccall wxRect_GetY      :: Ptr (TWxRect a) -> IO CInt
foreign import ccall wxRect_GetWidth  :: Ptr (TWxRect a) -> IO CInt
foreign import ccall wxRect_GetHeight :: Ptr (TWxRect a) -> IO CInt


{-----------------------------------------------------------------------------------------
  CInt
-----------------------------------------------------------------------------------------}
withIntResult :: IO CInt -> IO Int
withIntResult io
  = do x <- io
       return (fromCInt x)

toCInt :: Int -> CInt
toCInt i = fromIntegral i

fromCInt :: CInt -> Int
fromCInt ci
  = fromIntegral ci

{-----------------------------------------------------------------------------------------
  CIntPtr
-----------------------------------------------------------------------------------------}
withIntPtrResult :: IO CIntPtr -> IO IntPtr
withIntPtrResult io
  = do x <- io
       return (fromCIntPtr x)

toCIntPtr :: IntPtr -> CIntPtr
toCIntPtr i = fromIntegral i

fromCIntPtr :: CIntPtr -> IntPtr
fromCIntPtr ci
  = fromIntegral ci

{-----------------------------------------------------------------------------------------
  CDouble
-----------------------------------------------------------------------------------------}
withDoubleResult :: IO CDouble -> IO Double
withDoubleResult io
  = do x <- io
       return (fromCDouble x)

toCDouble :: Double -> CDouble
toCDouble d = realToFrac d

fromCDouble :: CDouble -> Double
fromCDouble cd
  = realToFrac cd

{-----------------------------------------------------------------------------------------
  CBool
-----------------------------------------------------------------------------------------}
#if __GLASGOW_HASKELL__ < 802
type CBool  = CInt
#endif

toCBool :: Bool -> CBool
toCBool = intToCBool . fromBool

withBoolResult :: IO CBool -> IO Bool
withBoolResult io
  = do x <- io
       return (fromCBool x)

fromCBool :: CBool -> Bool
fromCBool  = toBool . cboolToInt

foreign import ccall "intToBool" intToCBool :: Int -> CBool
foreign import ccall "boolToInt" cboolToInt :: CBool -> Int

{-----------------------------------------------------------------------------------------
  CString
-----------------------------------------------------------------------------------------}
withStringResult :: (Ptr CChar -> IO CInt) -> IO String
withStringResult f
  = do len <- f nullPtr
       if (len<=0)
        then return ""
        else withCString (replicate (fromCInt len) ' ') $ \cstr ->
             do _ <- f cstr
                peekCStringLen (cstr,fromCInt len)

withWStringResult :: (Ptr CWchar -> IO CInt) -> IO String
withWStringResult f
  = do len <- f nullPtr
       if (len<=0)
        then return ""
        else withCWString (replicate (fromCInt len) ' ') $ \cstr ->
             do _ <- f cstr
                peekCWString cstr

{-----------------------------------------------------------------------------------------
  ByteString
-----------------------------------------------------------------------------------------}
-- TODO: replace this by more efficient implementation.
-- e.g. use mmap when bytestring support mmap interface.
withByteStringResult :: (Ptr CChar -> IO CInt) -> IO B.ByteString
withByteStringResult f
  = do len <- f nullPtr
       if (len<=0)
        then return $ BC.pack ""
        else withCString (replicate (fromCInt len) ' ') $ \cstr ->
             do _ <- f cstr
                B.packCStringLen (cstr,fromCInt len)

withLazyByteStringResult :: (Ptr CChar -> IO CInt) -> IO LB.ByteString
withLazyByteStringResult f
  = do str <- withStringResult f
       return $ LBC.pack str

{-----------------------------------------------------------------------------------------
  Arrays
-----------------------------------------------------------------------------------------}
withArrayStringResult :: (Ptr (Ptr CChar) -> IO CInt) -> IO [String]
withArrayStringResult f
  = do clen <- f nullPtr
       let len = fromCInt clen
       if (len <= 0)
        then return []
        else allocaArray len $ \carr ->
             do _ <- f carr
                arr <- peekArray len carr
                mapM peekCString arr

-- FIXME: factorise with withArrayStringResult
withArrayWStringResult :: (Ptr (Ptr CWchar) -> IO CInt) -> IO [String]
withArrayWStringResult f
  = do clen <- f nullPtr
       let len = fromCInt clen
       if (len <= 0)
        then return []
        else allocaArray len $ \carr ->
             do _ <- f carr
                arr <- peekArray len carr
                mapM peekCWString arr


withArrayIntResult :: (Ptr CInt -> IO CInt) -> IO [Int]
withArrayIntResult f
  = do clen <- f nullPtr
       let len = fromCInt clen
       if (len <= 0)
        then return []
        else allocaArray len $ \carr ->
             do _ <- f carr
                xs <- peekArray len carr
                return (map fromCInt xs)

withArrayIntPtrResult :: (Ptr CIntPtr -> IO CInt) -> IO [IntPtr]
withArrayIntPtrResult f
  = do clen <- f nullPtr
       let len = fromCInt clen
       if (len <= 0)
        then return []
        else allocaArray len $ \carr ->
             do _ <- f carr
                xs <- peekArray len carr
                return (map fromCIntPtr xs)

withArrayObjectResult :: (Ptr (Ptr a) -> IO CInt) -> IO [Object a]
withArrayObjectResult f
  = do clen <- f nullPtr
       let len = fromCInt clen
       if (len <= 0)
        then return []
        else allocaArray len $ \carr ->
             do _ <- f carr
                ps <- peekArray len carr
                return (map objectFromPtr ps)

withArrayString :: [String] -> (CInt -> Ptr CString -> IO a) -> IO a
withArrayString xs f
  = withCStrings xs [] $ \cxs ->
    withArray0 ptrNull cxs $ \carr ->
    f (toCInt len) carr
  where
    len = length xs

    withCStrings [] cxs f'
      = f' (reverse cxs)
    withCStrings (x':xs') cxs f'
      = withCString x' $ \cx ->
        withCStrings xs' (cx:cxs) f'

-- FIXME: factorise with withArrayString
withArrayWString :: [String] -> (CInt -> Ptr CWString -> IO a) -> IO a
withArrayWString xs f
  = withCWStrings xs [] $ \cxs ->
    withArray0 ptrNull cxs $ \carr ->
    f (toCInt len) carr
  where
    len = length xs

    withCWStrings [] cxs f'
      = f' (reverse cxs)
    withCWStrings (x':xs') cxs f'
      = withCWString x' $ \cx ->
        withCWStrings xs' (cx:cxs) f'


withArrayInt :: [Int] -> (CInt -> Ptr CInt -> IO a) -> IO a
withArrayInt xs f
  = withArray0 0 (map toCInt xs) $ \carr ->
    f (toCInt (length xs)) carr

withArrayIntPtr :: [IntPtr] -> (CInt -> Ptr CIntPtr -> IO a) -> IO a
withArrayIntPtr xs f
  = withArray0 0 (map toCIntPtr xs) $ \carr ->
    f (toCInt (length xs)) carr

withArrayObject :: [Ptr a] -> (CInt -> Ptr (Ptr a) -> IO b) -> IO b
withArrayObject xs f
  = withArray0 ptrNull xs $ \carr ->
    f (toCInt (length xs)) carr

{-----------------------------------------------------------------------------------------
  CCHar
-----------------------------------------------------------------------------------------}
toCChar :: Char -> CChar
toCChar = castCharToCChar

-- generalised to work with Char and CChar
withCharResult :: (Integral a, Show a) => IO a -> IO Char
withCharResult io
  = do x <- io
       if (x < 0)
          then do traceIO ("Recieved negative unicode: " ++ (show x))
                  return '\n'
          else return (fromCWchar x)

{- The (x < 0) if expression in withCharResult is a workaround for
"processExecAsyncTimed dies with Prelude.chr bad argument"- bug
reported here
http://sourceforge.net/mailarchive/message.php?msg_id=54647.129.16.31.149.1111686341.squirrel%40webmail.chalmers.se
and here
http://www.mail-archive.com/wxhaskell-users@lists.sourceforge.net/msg00267.html

Windows GUI-only programs have no stdin, stdout or stderr. So we use Debug.Trace.traceIO
for reporting message.
http://www.haskell.org/ghc/docs/6.8.2/html/users_guide/terminal-interaction.html
http://www.haskell.org/ghc/docs/7.4.1/html/libraries/base-4.5.0.0/Debug-Trace.html#v:traceIO
-}


fromCChar :: CChar -> Char
fromCChar = castCCharToChar

{-----------------------------------------------------------------------------------------
  CCHar
-----------------------------------------------------------------------------------------}
toCWchar :: (Num a) => Char -> a
toCWchar = fromIntegral . fromEnum


fromCWchar :: Integral a => a -> Char
fromCWchar = toEnum . fromIntegral


{-----------------------------------------------------------------------------------------
  CFunPtr
-----------------------------------------------------------------------------------------}
toCFunPtr :: FunPtr a -> Ptr a
toCFunPtr fptr
  = castFunPtrToPtr fptr

-- | Null pointer, use with care.
ptrNull :: Ptr a
ptrNull
  = nullPtr

-- | Test for null.
ptrIsNull :: Ptr a -> Bool
ptrIsNull p
  = (p == ptrNull)

-- | Cast a pointer type, use with care.
ptrCast :: Ptr a -> Ptr b
ptrCast p
  = castPtr p

{-----------------------------------------------------------------------------------------
  Marshalling of classes that are managed
-----------------------------------------------------------------------------------------}
{-
-- | A @Managed a@ is a pointer to an object of type @a@, just like 'Object'. However,
-- managed objects are automatically deleted when garbage collected. This is used for
-- certain classes that are not managed by the wxWidgets library, like 'Bitmap's
type Managed a  = ForeignPtr a

-- | Create a managed object. Takes a finalizer as argument. This is normally a
-- a delete function like 'windowDelete'.
createManaged :: IO () -> Ptr a -> IO (Managed a)
createManaged final obj
  = newForeignPtr obj final

-- | Add an extra finalizer to a managed object.
managedAddFinalizer :: IO () -> Managed a -> IO ()
managedAddFinalizer io managed
  = addForeignPtrFinalizer managed io

-- | Do something with the object from a managed object.
withManaged :: Managed a -> (Ptr a -> IO b) -> IO b
withManaged fptr f
  = withForeignPtr fptr f


-- | Keep a managed object explicitly alive.
managedTouch :: Managed a -> IO ()
managedTouch fptr
  = touchForeignPtr fptr

-- | A null pointer, use with care.
{-# NOINLINE managedNull #-}
managedNull :: Managed a
managedNull
  = unsafePerformIO (createManaged (return ()) ptrNull)

-- | Test for null.
managedIsNull :: Managed a -> Bool
managedIsNull managed
  = (managed == managedNull)

-- | Cast a managed object, use with care.
managedCast :: Managed a -> Managed b
managedCast fptr
  = castForeignPtr fptr
-}


{-----------------------------------------------------------------------------------------
  Classes assigned by value.
-----------------------------------------------------------------------------------------}
assignRef :: IO (Ptr (TWxObject a)) -> (Ptr (TWxObject a) -> IO ()) -> IO (WxObject a)
assignRef create f
  = withManagedObjectResult (assignRefPtr create f)

assignRefPtr :: IO (Ptr a) -> (Ptr a -> IO ()) -> IO (Ptr a)
assignRefPtr create f
  = do p <- create
       f p
       return p


withManagedBitmapResult :: IO (Ptr (TBitmap a)) -> IO (Bitmap a)
withManagedBitmapResult io
  = do p      <- io
       static <- wxBitmap_IsStatic p
       if (static) 
        then return (objectFromPtr p)
        else do mp <- wxManagedPtr_CreateFromBitmap p
                objectFromManagedPtr mp

foreign import ccall wxManagedPtr_CreateFromBitmap :: Ptr (TBitmap a) -> IO (ManagedPtr (TBitmap a))
foreign import ccall wxBitmap_IsStatic :: Ptr (TBitmap a) -> IO Bool

withManagedIconResult :: IO (Ptr (TIcon a)) -> IO (Icon a)
withManagedIconResult io
  = do p      <- io
       if (wxIcon_IsStatic p) 
        then return (objectFromPtr p)
        else do mp <- wxManagedPtr_CreateFromIcon p
                objectFromManagedPtr mp

foreign import ccall wxManagedPtr_CreateFromIcon :: Ptr (TIcon a) -> IO (ManagedPtr (TIcon a))
foreign import ccall wxIcon_IsStatic :: Ptr (TIcon a) -> Bool

withManagedBrushResult :: IO (Ptr (TBrush a)) -> IO (Brush a)
withManagedBrushResult io
  = do p      <- io
       if (wxBrush_IsStatic p) 
        then return (objectFromPtr p)
        else do mp <- wxManagedPtr_CreateFromBrush p
                objectFromManagedPtr mp

foreign import ccall wxManagedPtr_CreateFromBrush :: Ptr (TBrush a) -> IO (ManagedPtr (TBrush a))
foreign import ccall wxBrush_IsStatic :: Ptr (TBrush a) -> Bool

withManagedCursorResult :: IO (Ptr (TCursor a)) -> IO (Cursor a)
withManagedCursorResult io
  = do p      <- io
       if (wxCursor_IsStatic p) 
        then return (objectFromPtr p)
        else do mp <- wxManagedPtr_CreateFromCursor p
                objectFromManagedPtr mp

foreign import ccall wxManagedPtr_CreateFromCursor :: Ptr (TCursor a) -> IO (ManagedPtr (TCursor a))
foreign import ccall wxCursor_IsStatic :: Ptr (TCursor a) -> Bool

withManagedFontResult :: IO (Ptr (TFont a)) -> IO (Font a)
withManagedFontResult io
  = do p      <- io
       if (wxFont_IsStatic p) 
        then return (objectFromPtr p)
        else do mp <- wxManagedPtr_CreateFromFont p
                objectFromManagedPtr mp

foreign import ccall wxManagedPtr_CreateFromFont :: Ptr (TFont a) -> IO (ManagedPtr (TFont a))
foreign import ccall wxFont_IsStatic :: Ptr (TFont a) -> Bool

withManagedPenResult :: IO (Ptr (TPen a)) -> IO (Pen a)
withManagedPenResult io
  = do p      <- io
       if (wxPen_IsStatic p) 
        then return (objectFromPtr p)
        else do mp <- wxManagedPtr_CreateFromPen p
                objectFromManagedPtr mp

foreign import ccall wxManagedPtr_CreateFromPen :: Ptr (TPen a) -> IO (ManagedPtr (TPen a))
foreign import ccall wxPen_IsStatic :: Ptr (TPen a) -> Bool



withRefBitmap :: (Ptr (TBitmap a) -> IO ()) -> IO (Bitmap a)
withRefBitmap f
  = withManagedBitmapResult $ assignRefPtr wxBitmap_Create  f
foreign import ccall "wxBitmap_CreateDefault" wxBitmap_Create :: IO (Ptr (TBitmap a))

withRefCursor :: (Ptr (TCursor a) -> IO ()) -> IO (Cursor a)
withRefCursor f
  = withManagedCursorResult $ assignRefPtr (wx_Cursor_CreateFromStock 1)  f
foreign import ccall "Cursor_CreateFromStock" wx_Cursor_CreateFromStock :: CInt -> IO (Ptr (TCursor a))

withRefIcon :: (Ptr (TIcon a) -> IO ()) -> IO (Icon a)
withRefIcon f
  = withManagedIconResult $ assignRefPtr wxIcon_Create  f
foreign import ccall "wxIcon_CreateDefault" wxIcon_Create :: IO (Ptr (TIcon a))

withRefImage :: (Ptr (TImage a) -> IO ()) -> IO (Image a)
withRefImage f
  = assignRef wxImage_Create  f
foreign import ccall "wxImage_CreateDefault" wxImage_Create :: IO (Ptr (TImage a))

withRefFont :: (Ptr (TFont a) -> IO ()) -> IO (Font a)
withRefFont f
  = withManagedFontResult $ assignRefPtr wxFont_Create  f
foreign import ccall "wxFont_CreateDefault" wxFont_Create :: IO (Ptr (TFont a))


withRefPen :: (Ptr (TPen a) -> IO ()) -> IO (Pen a)
withRefPen f
  = withManagedPenResult $ assignRefPtr wxPen_Create  f
foreign import ccall "wxPen_CreateDefault" wxPen_Create :: IO (Ptr (TPen a))


withRefBrush :: (Ptr (TBrush a) -> IO ()) -> IO (Brush a)
withRefBrush f
  = withManagedBrushResult $ assignRefPtr wxBrush_Create  f
foreign import ccall "wxBrush_CreateDefault" wxBrush_Create :: IO (Ptr (TBrush a))

withRefFontData :: (Ptr (TFontData a) -> IO ()) -> IO (FontData a)
withRefFontData f
  = assignRef wxFontData_Create  f
foreign import ccall "wxFontData_Create" wxFontData_Create :: IO (Ptr (TFontData a))

withRefListItem :: (Ptr (TListItem a) -> IO ()) -> IO (ListItem a)
withRefListItem f
  = assignRef wxListItem_Create  f
foreign import ccall "wxListItem_Create" wxListItem_Create :: IO (Ptr (TListItem a))

withRefPrintData :: (Ptr (TPrintData a) -> IO ()) -> IO (PrintData a)
withRefPrintData f
  = assignRef wxPrintData_Create  f
foreign import ccall "wxPrintData_Create" wxPrintData_Create :: IO (Ptr (TPrintData a))

withRefPrintDialogData :: (Ptr (TPrintDialogData a) -> IO ()) -> IO (PrintDialogData a)
withRefPrintDialogData f
  = assignRef wxPrintDialogData_Create  f
foreign import ccall "wxPrintDialogData_CreateDefault" wxPrintDialogData_Create :: IO (Ptr (TPrintDialogData a))

withRefPageSetupDialogData :: (Ptr (TPageSetupDialogData a) -> IO ()) -> IO (PageSetupDialogData a)
withRefPageSetupDialogData f
  = assignRef wxPageSetupDialogData_Create  f
foreign import ccall "wxPageSetupDialogData_Create" wxPageSetupDialogData_Create :: IO (Ptr (TPageSetupDialogData a))


withManagedDateTimeResult :: IO (Ptr (TDateTime a)) -> IO (DateTime a)
withManagedDateTimeResult io
  = do p  <- io
       if (p==nullPtr)
        then return (objectFromPtr p)
        else do mp <- wxManagedPtr_CreateFromDateTime p
                objectFromManagedPtr mp

foreign import ccall wxManagedPtr_CreateFromDateTime :: Ptr (TDateTime a) -> IO (ManagedPtr (TDateTime a))


withRefDateTime :: (Ptr (TDateTime a) -> IO ()) -> IO (DateTime a)
withRefDateTime f
  = withManagedDateTimeResult $ assignRefPtr wxDateTime_Create  f
foreign import ccall "wxDateTime_Create" wxDateTime_Create :: IO (Ptr (TDateTime a))


withManagedGridCellCoordsArrayResult :: IO (Ptr (TGridCellCoordsArray a)) -> IO (GridCellCoordsArray a)
withManagedGridCellCoordsArrayResult io
  = do p  <- io
       if (p==nullPtr)
        then return (objectFromPtr p)
        else do mp <- wxManagedPtr_CreateFromGridCellCoordsArray p
                objectFromManagedPtr mp

foreign import ccall wxManagedPtr_CreateFromGridCellCoordsArray :: Ptr (TGridCellCoordsArray a) -> IO (ManagedPtr (TGridCellCoordsArray a))

withRefGridCellCoordsArray :: (Ptr (TGridCellCoordsArray a) -> IO ()) -> IO (GridCellCoordsArray a)
withRefGridCellCoordsArray f
  = withManagedGridCellCoordsArrayResult $ assignRefPtr wxGridCellCoordsArray_Create  f
foreign import ccall "wxGridCellCoordsArray_Create" wxGridCellCoordsArray_Create :: IO (Ptr (TGridCellCoordsArray a))




{-----------------------------------------------------------------------------------------
  Tree items
-----------------------------------------------------------------------------------------}
-- | Identifies tree items. Note: Replaces the @TreeItemId@ object and takes automatically
-- care of allocation issues.
newtype TreeItem  = TreeItem IntPtr
                  deriving (Eq,Show,Read)

-- | Invalid tree item.
treeItemInvalid :: TreeItem
treeItemInvalid   = TreeItem 0

-- | Is a tree item ok? (i.e. not invalid).
treeItemIsOk :: TreeItem -> Bool
treeItemIsOk (TreeItem val)
  = (val /= 0)

treeItemFromInt :: IntPtr -> TreeItem
treeItemFromInt i
  = TreeItem i

withRefTreeItemId :: (Ptr (TTreeItemId ()) -> IO ()) -> IO TreeItem
withRefTreeItemId f
  = do item <- assignRefPtr treeItemIdCreate f
       val  <- treeItemIdGetValue item
       treeItemIdDelete item
       return (TreeItem (fromCIntPtr val))

withTreeItemIdRef :: String -> TreeItem -> (Ptr (TTreeItemId a) -> IO b) -> IO b
withTreeItemIdRef msg t f
  = withTreeItemIdPtr t $ \p -> withValidPtr msg p f

withTreeItemIdPtr :: TreeItem -> (Ptr (TTreeItemId a) -> IO b) -> IO b
withTreeItemIdPtr (TreeItem val) f 
  = do item <- treeItemIdCreateFromValue (toCIntPtr val)
       x    <- f item
       treeItemIdDelete item
       return x

withManagedTreeItemIdResult :: IO (Ptr (TTreeItemId a)) -> IO TreeItem 
withManagedTreeItemIdResult io
  = do item <- io
       val  <- treeItemIdGetValue item
       treeItemIdDelete item
       return (TreeItem (fromCIntPtr val))

foreign import ccall "wxTreeItemId_Create" treeItemIdCreate :: IO (Ptr (TTreeItemId a))
foreign import ccall "wxTreeItemId_GetValue" treeItemIdGetValue :: Ptr (TTreeItemId a) -> IO CIntPtr
foreign import ccall "wxTreeItemId_CreateFromValue" treeItemIdCreateFromValue :: CIntPtr -> IO (Ptr (TTreeItemId a))
foreign import ccall "wxTreeItemId_Delete" treeItemIdDelete :: Ptr (TTreeItemId a) -> IO ()

{-----------------------------------------------------------------------------------------
  String
-----------------------------------------------------------------------------------------}
{-
-- | A @wxString@ object.
type WxStringObject a   = Ptr (CWxStringObject a)
type TWxStringObject a  = CWxStringObject a
data CWxStringObject a  = CWxStringObject
-}

-- FIXME: I am blithely changing these over to use CWString instead of String
-- whereas in the rest of the code, I actually make a new version of the fns
withStringRef :: String -> String -> (Ptr (TWxString s) -> IO a) -> IO a
withStringRef msg s f
  = withStringPtr s $ \p -> withValidPtr msg p f

withStringPtr :: String -> (Ptr (TWxString s) -> IO a) -> IO a
withStringPtr s f
  = withCWString s $ \cstr ->
    bracket (wxString_Create cstr)
            (wxString_Delete)
            f

withManagedStringResult :: IO (Ptr (TWxString a)) -> IO String
withManagedStringResult io
  = do wxs <- io
       len <- wxString_Length wxs
       s   <- if (len<=0)
                then return ""
                else withCWString (replicate (fromCInt len) ' ') $ \cstr ->
                     do _ <- wxString_GetString wxs cstr
                        peekCWStringLen (cstr, fromCInt len)
       wxString_Delete wxs
       return s


foreign import ccall wxString_Create    :: Ptr CWchar -> IO (Ptr (TWxString a))
-- foreign import ccall wxString_CreateLen :: Ptr CWchar -> CInt -> IO (Ptr (TWxString a))
foreign import ccall wxString_Delete    :: Ptr (TWxString a) -> IO ()
foreign import ccall wxString_GetString :: Ptr (TWxString a) -> Ptr CWchar -> IO CInt
foreign import ccall wxString_Length    :: Ptr (TWxString a) -> IO CInt


{-----------------------------------------------------------------------------------------
  Color
-----------------------------------------------------------------------------------------}
-- | An abstract data type to define colors.
newtype Color = Color Word 
              deriving (Eq, Typeable) -- , IArray UArray) 

instance Show Color where
  showsPrec d c
    = showParen (d > 0) (showString "rgba(" . shows (colorRed   c :: Int) .
                          showChar   ','    . shows (colorGreen c :: Int) .
                          showChar   ','    . shows (colorBlue  c :: Int) .
                          showChar   ','    . shows (colorAlpha c :: Int) .
                          showChar   ')' )

-- | Create a color from a red\/green\/blue triple.
colorRGB :: (Integral a) => a -> a -> a -> Color
colorRGB r g b = Color (shiftL (fromIntegral r) 24 .|. shiftL (fromIntegral g) 16 .|. shiftL (fromIntegral b) 8 .|. 255)

-- | Create a color from a red\/green\/blue triple.
rgb :: (Integral a) => a -> a -> a -> Color
rgb r g b = colorRGB r g b

-- | Create a color from a red\/green\/blue\/alpha quadruple.
colorRGBA :: (Integral a) => a -> a -> a -> a -> Color
colorRGBA r g b a = Color (shiftL (fromIntegral r) 24 .|. shiftL (fromIntegral g) 16 .|. shiftL (fromIntegral b) 8 .|. (fromIntegral a))

-- | Create a color from a red\/green\/blue\/alpha quadruple.
rgba :: (Integral a) => a -> a -> a -> a -> Color
rgba r g b a = colorRGBA r g b a


-- | Return an 'Int' where the three least significant bytes contain
-- the red, green, and blue component of a color.
intFromColor :: Color -> Int
intFromColor colr
  = let r = colorRed colr
        g = colorGreen colr
        b = colorBlue colr
    in (shiftL r 16 .|. shiftL g 8 .|. b)

-- | Set the color according to an rgb integer. (see 'rgbIntFromColor').
colorFromInt :: Int -> Color
colorFromInt colr
  = let r = (shiftR colr 16) .&. 0xFF
        g = (shiftR colr 8) .&. 0xFF
        b = colr .&. 0xFF
    in colorRGB r g b

-- | Return an 'Num' class's numeric representation where the three
-- least significant the red, green, and blue component of a color.
fromColor :: (Num a) => Color -> a
fromColor (Color colr)
  = fromIntegral colr

-- | Set the color according to 'Integral' class's numeric representation.
-- (see 'rgbaIntFromColor').
toColor :: (Integral a) => a -> Color
toColor
  = Color . fromIntegral

-- marshalling 1
-- | Returns a red color component
colorRed   :: (Num a) => Color -> a
colorRed   (Color colr) = fromIntegral ((shiftR colr 24) .&. 0xFF)

-- | Returns a green color component
colorGreen :: (Num a) => Color -> a
colorGreen (Color colr) = fromIntegral ((shiftR colr 16) .&. 0xFF)

-- | Returns a blue color component
colorBlue  :: (Num a) => Color -> a
colorBlue  (Color colr) = fromIntegral ((shiftR colr 8) .&. 0xFF)

-- | Returns a alpha channel component
colorAlpha  :: (Num a) => Color -> a
colorAlpha  (Color colr) = fromIntegral (colr .&. 0xFF)


-- | This is an illegal color, corresponding to @nullColour@.
colorNull :: Color
colorNull
  = Color (maxBound)

{-# DEPRECATED colorOk "Use colorIsOk instead" #-}
-- | deprecated: use 'colorIsOk' instead.
colorOk :: Color -> Bool
colorOk = colorIsOk

-- | Check of a color is valid (@Colour::IsOk@)
colorIsOk :: Color -> Bool
colorIsOk = (/= colorNull)


-- marshalling 2
{-
type Colour a     = Object (CColour a)
type ColourPtr a  = Ptr (CColour a)
data CColour a    = CColour
-}

withRefColour :: (Ptr (TColour a) -> IO ()) -> IO Color
withRefColour f
  = withManagedColourResult $
    assignRefPtr colourCreate f

withManagedColourResult :: IO (Ptr (TColour a)) -> IO Color
withManagedColourResult io
  = do pcolour <- io
       color <- do ok <- colourIsOk pcolour
                   if (ok==0)
                    then return colorNull
                    else do colr <- colourGetUnsignedInt pcolour
                            return (toColor colr)
       colourSafeDelete pcolour
       return color


withColourRef :: String -> Color -> (Ptr (TColour a) -> IO b) -> IO b
withColourRef msg c f
  = withColourPtr c $ \p -> withValidPtr msg p f

withColourPtr :: Color -> (Ptr (TColour a) -> IO b) -> IO b
withColourPtr c f
  = do pcolour <- colourCreateFromUnsignedInt (fromColor c)
       x <- f pcolour
       colourSafeDelete pcolour
       return x

colourFromColor :: Color -> IO (Colour ())
colourFromColor c
  = if (colorOk c)
     then do p <- colourCreateFromUnsignedInt (fromColor c)
             if (colourIsStatic p)
              then return (objectFromPtr p)
              else do mp <- wxManagedPtr_CreateFromColour p
                      objectFromManagedPtr mp
     else withObjectResult colourNull
          

colorFromColour :: Colour a -> IO Color
colorFromColour c
  = withObjectRef "colorFromColour" c $ \pcolour ->
    do ok <- colourIsOk pcolour
       if (ok==0)
        then return colorNull
        else do colr <- colourGetUnsignedInt pcolour
                return (toColor colr)


foreign import ccall "wxColour_CreateEmpty" colourCreate    :: IO (Ptr (TColour a))
-- foreign import ccall "wxColour_CreateFromInt" colourCreateFromInt :: CInt -> IO (Ptr (TColour a))
-- foreign import ccall "wxColour_GetInt" colourGetInt               :: Ptr (TColour a) -> IO CInt
foreign import ccall "wxColour_CreateFromUnsignedInt" colourCreateFromUnsignedInt :: Word -> IO (Ptr (TColour a))
foreign import ccall "wxColour_GetUnsignedInt" colourGetUnsignedInt       :: Ptr (TColour a) -> IO Word
foreign import ccall "wxColour_SafeDelete" colourSafeDelete   :: Ptr (TColour a) -> IO ()
foreign import ccall "wxColour_IsStatic" colourIsStatic   :: Ptr (TColour a) -> Bool
foreign import ccall "wxColour_IsOk"    colourIsOk   :: Ptr (TColour a) -> IO CInt
foreign import ccall "Null_Colour"    colourNull :: IO (Ptr (TColour a))
foreign import ccall wxManagedPtr_CreateFromColour :: Ptr (TColour a) -> IO (ManagedPtr (TColour a))
