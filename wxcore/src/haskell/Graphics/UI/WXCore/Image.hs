{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
{-|
Module      :  Image
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable
-}
--------------------------------------------------------------------------------
module Graphics.UI.WXCore.Image
    ( -- * Images
      topLevelWindowSetIconFromFile
    -- ** Imagelist
    , imageListAddIconsFromFiles
    , imageListAddIconFromFile
    -- ** Icons
    , withIconFromFile
    , iconCreateFromFile
    , iconGetSize
    -- ** Cursors
    , withCursorFromFile
    , cursorCreateFromFile
    -- ** Bitmaps
    , withBitmapFromFile
    , bitmapCreateFromFile
    , bitmapGetSize
    , bitmapSetSize
    -- ** Helpers
    , imageTypeFromExtension
    , imageTypeFromFileName

    -- * Direct image manipulation
    , imageGetPixels
    , imageCreateFromPixels
    , imageGetPixelArray
    , imageCreateFromPixelArray
    , imageGetSize
    , withImageData

    -- ** Pixel buffer
    , imageCreateFromPixelBuffer
    , imageGetPixelBuffer
    , withPixelBuffer
    , PixelBuffer
    , pixelBufferCreate
    , pixelBufferDelete
    , pixelBufferInit
    , pixelBufferSetPixel
    , pixelBufferGetPixel    
    , pixelBufferSetPixels
    , pixelBufferGetPixels    
    , pixelBufferGetSize
    ) where

import Data.Char( toLower )
import Data.Array.IArray ( IArray, listArray, bounds, elems )
import Foreign.Marshal.Array

import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types


{-----------------------------------------------------------------------------------------
  ImageList
-----------------------------------------------------------------------------------------}
-- | Initialize an image list with icons from files. Use a 'sizeNull' to
-- use the native size of the loaded icons.
imageListAddIconsFromFiles :: ImageList a -> Size -> [FilePath] -> IO ()
imageListAddIconsFromFiles images desiredSize fnames
  = mapM_ (imageListAddIconFromFile images desiredSize) fnames

-- | Add an icon from a file to an imagelist.
imageListAddIconFromFile :: ImageList a -> Size -> FilePath -> IO ()
imageListAddIconFromFile images desiredSize fname
  = do image <- imageCreateFromFile fname
       imageRescale image desiredSize
       bitmap <- imageConvertToBitmap image
       _ <- imageListAddBitmap images bitmap nullBitmap
       bitmapDelete bitmap
       imageDelete image
       return ()

{-----------------------------------------------------------------------------------------
  Icons
-----------------------------------------------------------------------------------------}

-- | Set the icon of a frame.
topLevelWindowSetIconFromFile :: TopLevelWindow a -> FilePath -> IO ()
topLevelWindowSetIconFromFile f fname
  = withIconFromFile fname sizeNull (topLevelWindowSetIcon f)


-- | Load an icon (see 'iconCreateFromFile') and automatically delete it
-- after use.
withIconFromFile :: FilePath -> Size -> (Icon () -> IO a) -> IO a
withIconFromFile fname size f
  = bracket (iconCreateFromFile fname size)
            (iconDelete)
            f

-- | Load an icon from an icon file (ico,xbm,xpm,gif). The 'Size' argument
-- gives the desired size but can be 'sizeNull' to retrieve the image
-- in its natural size. 
iconCreateFromFile :: FilePath -> Size -> IO (Icon ())
iconCreateFromFile fname size
  = iconCreateLoad fname (imageTypeFromFileName fname) size
  {-
    do icon <- iconCreateLoad fname (imageTypeFromFileName fname) size
       ok   <- iconOk icon
       if (ok)
        then return icon
        else do iconDelete icon
                ioError (userError ("unable to load icon: " ++ show fname))
  -}

-- | Get the size of an icon.
iconGetSize :: Icon a -> IO Size
iconGetSize icon
  = do w <- iconGetWidth icon
       h <- iconGetHeight icon
       return (sz w h)

{-----------------------------------------------------------------------------------------
  Cursors
-----------------------------------------------------------------------------------------}
-- | Load a cursor (see 'cursorCreateFromFile') and automatically delete it
-- after use.
withCursorFromFile :: FilePath -> (Cursor () -> IO a) -> IO a
withCursorFromFile fname f
  = bracket (cursorCreateFromFile fname)
            (cursorDelete)
            f

-- | Load a cursor from an icon file (ico,xbm,xpm,gif).
-- For a reason, this function is incompatible with 'iconCreateFromFile'.
cursorCreateFromFile :: String -> IO (Cursor ())
cursorCreateFromFile fname = imageCreateFromFile fname >>= cursorCreateFromImage

{-----------------------------------------------------------------------------------------
  Bitmaps
-----------------------------------------------------------------------------------------}

-- | Load a bitmap (see 'bitmapCreateFromFile') and automatically delete it
-- after use.
withBitmapFromFile :: FilePath -> (Bitmap () -> IO a) -> IO a
withBitmapFromFile fname f
  = bracket (bitmapCreateFromFile fname)
            (bitmapDelete)
            f

-- | Load a bitmap from an image file (gif, jpg, png, etc.) 
bitmapCreateFromFile :: FilePath -> IO (Bitmap ())
bitmapCreateFromFile fname
  = bitmapCreateLoad fname (imageTypeFromFileName fname)
    {-
    do bm <- bitmapCreateLoad fname (imageTypeFromFileName fname)
       ok <- bitmapOk bm
       if (ok)
        then return bm
        else do bitmapDelete bm
                ioError (userError ("unable to load image: " ++ show fname))
    -}

-- | The size of a bitmap.
bitmapGetSize :: Bitmap a -> IO Size
bitmapGetSize bitmap
  = do w <- bitmapGetWidth bitmap
       h <- bitmapGetHeight bitmap
       return (sz w h)

-- | Set the size of a bitmap.
bitmapSetSize :: Bitmap a -> Size -> IO ()
bitmapSetSize bitmap (Size w h)
  = do bitmapSetWidth bitmap w
       bitmapSetHeight bitmap h


{-----------------------------------------------------------------------------------------
  Images
-----------------------------------------------------------------------------------------}

-- | Get an image type from a file name.
imageTypeFromFileName :: String -> BitFlag
imageTypeFromFileName fname
  = imageTypeFromExtension (map toLower (reverse (takeWhile (/= '.') (reverse fname))))

-- | Get an image type from a file extension.
imageTypeFromExtension :: String -> BitFlag
imageTypeFromExtension ext
  = case ext of
      "jpg"   -> wxBITMAP_TYPE_JPEG
      "jpeg"  -> wxBITMAP_TYPE_JPEG
      "gif"   -> wxBITMAP_TYPE_GIF
      "bmp"   -> wxBITMAP_TYPE_BMP
      "png"   -> wxBITMAP_TYPE_PNG
      "xpm"   -> wxBITMAP_TYPE_XPM
      "xbm"   -> wxBITMAP_TYPE_XBM
      "pcx"   -> wxBITMAP_TYPE_PCX
      "ico"   -> wxBITMAP_TYPE_ICO
      "tif"   -> wxBITMAP_TYPE_TIF
      "tiff"  -> wxBITMAP_TYPE_TIF
      "pnm"   -> wxBITMAP_TYPE_PNM
      "pict"  -> wxBITMAP_TYPE_PICT
      "icon"  -> wxBITMAP_TYPE_ICON
      "ani"   -> wxBITMAP_TYPE_ANI
      _other  -> wxBITMAP_TYPE_ANY

{-----------------------------------------------------------------------------------------
  Direct image manipulation
-----------------------------------------------------------------------------------------}
-- | An abstract pixel buffer (= array of RGB values)
data PixelBuffer   = PixelBuffer Bool Size (Ptr Word8)

-- | Create a pixel buffer. (To be deleted with 'pixelBufferDelete').
pixelBufferCreate   :: Size -> IO PixelBuffer
pixelBufferCreate size
  = do buffer <- wxcMalloc (sizeW size * sizeH size * 3)
       return (PixelBuffer True size (ptrCast buffer))

-- | Delete a pixel buffer. 
pixelBufferDelete  :: PixelBuffer -> IO ()
pixelBufferDelete (PixelBuffer owned _size buffer)
  = when (owned && not (ptrIsNull buffer)) (wxcFree buffer)

-- | The size of a pixel buffer
pixelBufferGetSize :: PixelBuffer -> Size
pixelBufferGetSize (PixelBuffer _owned size _buffer)
  = size

-- | Get all the pixels of a pixel buffer as a single list.
pixelBufferGetPixels :: PixelBuffer -> IO [Color]
pixelBufferGetPixels (PixelBuffer _owned (Size w h) buffer)
  = do let count = w*h
       rgbs <- peekArray (3*count) buffer
       return (convert rgbs)
  where
    convert :: [Word8] -> [Color]
    convert (r:g:b:xs)  = colorRGB r g b : convert xs
    convert []          = []
    convert _           = 
      error $ "Graphics.UI.WXCore.Image.pixelBufferGetPixels: " ++
              "Unexpected number of entries in pixelbuffer"

-- | Set all the pixels of a pixel buffer.
pixelBufferSetPixels :: PixelBuffer -> [Color] -> IO ()
pixelBufferSetPixels (PixelBuffer _owned (Size w h) buffer) colors
  = do let count = w*h
       pokeArray buffer (convert (take count colors))
  where
    convert :: [Color] -> [Word8]
    convert (c:cs) = colorRed c : colorGreen c : colorBlue c : convert cs
    convert []     = []

-- | Initialize the pixel buffer with a grey color. The second argument
-- specifies the /greyness/ as a number between 0.0 (black) and 1.0 (white).
pixelBufferInit :: PixelBuffer -> Color -> IO ()
pixelBufferInit (PixelBuffer _owned size buffer) color
  = wxcInitPixelsRGB buffer size (intFromColor color)

-- | Set the color of a pixel.
pixelBufferSetPixel :: PixelBuffer -> Point -> Color -> IO ()
pixelBufferSetPixel (PixelBuffer _owned size buffer) poynt color
  = {-
    do let idx = 3*(y*w + x)
           r   = colorRed color
           g   = colorGreen color
           b   = colorBlue color
       pokeByteOff buffer idx r
       pokeByteOff buffer (idx+1) g
       pokeByteOff buffer (idx+2) b
    -}
    wxcSetPixelRGB buffer (sizeW size) poynt (intFromColor color)
    

-- | Get the color of a pixel
pixelBufferGetPixel :: PixelBuffer -> Point -> IO Color
pixelBufferGetPixel (PixelBuffer _owned size buffer) poynt
  = {-
    do let idx = 3*(y*w + x)
       r   <- peekByteOff buffer idx
       g   <- peekByteOff buffer (idx+1)
       b   <- peekByteOff buffer (idx+2)
       return (colorRGB r g b)
    -}
    do colr <- wxcGetPixelRGB buffer (sizeW size) poynt
       return (colorFromInt colr)
      
  
-- | Create an image from a pixel buffer. Note: the image will
-- delete the pixelbuffer.
imageCreateFromPixelBuffer :: PixelBuffer -> IO (Image ())
imageCreateFromPixelBuffer (PixelBuffer _owned size buffer) 
  = imageCreateFromDataEx size buffer False

-- | Do something with the pixels of an image
withImageData :: Image a -> (Ptr () -> IO b) -> IO b
withImageData image f = do
    pixels <- imageGetData image
    x <- f pixels
    image `seq` return x -- about this seq:
    -- it's not that we're trying to force evaluation order;
    -- we merely want to prevent image from being garbage
    -- collected before we've managed to use the array that
    -- is being pointed to

withPixelBuffer :: Image a -> (PixelBuffer -> IO b) -> IO b
withPixelBuffer image f =
    withImageData image $ \ptr -> do
       w   <- imageGetWidth image
       h   <- imageGetHeight image
       f $ PixelBuffer False (sz w h) (ptrCast ptr)

{-# DEPRECATED imageGetPixelBuffer "Use withPixelBuffer instead" #-}
-- | Get the pixel buffer of an image.
--   Note: use 'withPixelBuffer' instead
imageGetPixelBuffer :: Image a -> IO PixelBuffer
imageGetPixelBuffer image
  = withPixelBuffer image return

-- | Get the pixels of an image.
imageGetPixels :: Image a -> IO [Color]
imageGetPixels image
  = withPixelBuffer image pixelBufferGetPixels

-- | Create an image from a list of pixels.
imageCreateFromPixels :: Size -> [Color] -> IO (Image ())
imageCreateFromPixels size colors
  = do pb <- pixelBufferCreate size
       pixelBufferSetPixels pb colors
       imageCreateFromPixelBuffer pb   -- image deletes pixel buffer

-- | Get the pixels of an image as an array
imageGetPixelArray :: (IArray a Color) => Image b -> IO (a Point Color)
imageGetPixelArray image
  = do h  <- imageGetHeight image
       w  <- imageGetWidth image
       ps <- imageGetPixels image
       let bounds' = (pointZero, point (w-1) (h-1))
       return (listArray bounds' ps)        

-- | Create an image from a pixel array
imageCreateFromPixelArray :: (IArray a Color) => a Point Color -> IO (Image ())
imageCreateFromPixelArray pixels
  = let (Point x y) = snd (bounds pixels)
    in imageCreateFromPixels (sz (x+1) (y+1)) (elems pixels)

-- | Get the size of an image
imageGetSize :: Image a -> IO Size
imageGetSize image
  = do h  <- imageGetHeight image
       w  <- imageGetWidth image
       return (Size w h)
