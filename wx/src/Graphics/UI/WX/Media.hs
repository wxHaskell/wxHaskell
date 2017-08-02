{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
{-|
Module      :  Media
Copyright   :  (c) Daan Leijen 2003
               (c) shelarcy 2007
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Images, Media, Sounds, and action!
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Media
            ( -- * Media
              Media(..)
            
              -- * Sound
            , sound, playLoop, playWait
              -- * Images
            , image, imageCreateFromFile, imageCreateFromPixels, imageGetPixels
            , imageCreateFromPixelArray, imageGetPixelArray
            
              -- * Bitmaps
            , bitmap, bitmapCreateFromFile, bitmapFromImage
            ) where

import System.IO.Unsafe( unsafePerformIO )
import Graphics.UI.WXCore 
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes

{--------------------------------------------------------------------
  Bitmaps
--------------------------------------------------------------------}
-- | Return a managed bitmap object. Bitmaps are abstract images used
-- for drawing to a device context. The file path should point to
-- a valid bitmap file, normally a @.ico@, @.bmp@, @.xpm@, or @.png@,
-- but any file format supported by 'Image' is correctly loaded.
--
-- Instances: 'Sized'.
bitmap :: FilePath -> Bitmap ()
bitmap fname
  = unsafePerformIO $ bitmapCreateFromFile fname

instance Sized (Bitmap a) where
  size  = newAttr "size" bitmapGetSize bitmapSetSize

-- | Create a bitmap from an image with the same color depth.
bitmapFromImage :: Image a -> IO (Bitmap ())
bitmapFromImage image'
  = bitmapCreateFromImage image' (-1)

{--------------------------------------------------------------------
  Images
--------------------------------------------------------------------}
-- | Return a managed image. Images are platform independent representations
-- of pictures, using an array of rgb pixels. See "Graphics.UI.WXCore.Image" for
-- low-level pixel manipulation. The file path should point to
-- a valid image file, like @.jpg@, @.bmp@, @.xpm@, or @.png@, for example.
--
-- Instances: 'Sized'.
image :: FilePath -> Image ()
image fname
  = unsafePerformIO $ imageCreateFromFile fname

instance Sized (Image a) where
  size  = newAttr "size" imageGetSize imageRescale

{--------------------------------------------------------------------
  Media
--------------------------------------------------------------------}
-- | Abstract layer between 'MediaCtrl' and 'Sound'. This class intends to
-- avoid breaking backward-compatibility.
class Media w where
  -- | If use this method with 'Sound', play a sound fragment asynchronously.
  -- If use this method with 'MediaCtrl', play media that is loaded by
  -- 'mediaCtrlLoad'.
  play  :: w -> IO ()
  stop  :: w -> IO ()

{--------------------------------------------------------------------
  Sounds
--------------------------------------------------------------------}
-- | Return a managed sound object. The file path points to 
-- a valid sound file, normally a @.wav@.
sound :: FilePath -> Sound ()
sound fname 
  = unsafePerformIO $ soundCreate fname False

instance Media (Sound a) where
  play sound' = unitIO (soundPlay sound' wxSOUND_ASYNC)
  stop = soundStop

-- | Play a sound fragment repeatedly (and asynchronously).
playLoop :: Sound a -> IO ()
playLoop sound'
  = unitIO (soundPlay sound' $ wxSOUND_ASYNC .+. wxSOUND_LOOP)

-- | Play a sound fragment synchronously (i.e. wait till completion).
playWait :: Sound a -> IO ()
playWait sound'
  = unitIO (soundPlay sound' wxSOUND_SYNC)

