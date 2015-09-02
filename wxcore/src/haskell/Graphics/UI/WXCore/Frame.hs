-----------------------------------------------------------------------------------------
{-|
Module      :  Frame
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Frame utility functions.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Frame
        ( -- * Frame
          frameCreateTopFrame
        , frameCreateDefault
        , frameSetTopFrame
        , frameDefaultStyle
        , frameCenter
        , frameCenterHorizontal
        , frameCenterVertical
          -- * Window
        , windowGetRootParent
        , windowGetFrameParent
        , windowGetMousePosition
        , windowGetScreenPosition
        , windowChildren
          -- * Dialog
        , dialogDefaultStyle
          -- * Status bar
        , statusBarCreateFields
        ) where

import Data.Bits
import Foreign.Marshal.Array
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClassInfo
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types


-- | The default frame style for a normal top-level 'Frame'.
frameDefaultStyle :: Style
frameDefaultStyle
  = wxDEFAULT_FRAME_STYLE .|. wxCLIP_CHILDREN -- .|. wxNO_FULL_REPAINT_ON_RESIZE 

-- | The default frame style for a normal 'Dialog'.
dialogDefaultStyle :: Style
dialogDefaultStyle
  = wxCAPTION .|. wxSYSTEM_MENU .|. wxTAB_TRAVERSAL .|. wxCLOSE_BOX .|. wxCLIP_CHILDREN 
    -- .|. wxNO_FULL_REPAINT_ON_RESIZE 

------------------------------------------------------------------------------------------
--
------------------------------------------------------------------------------------------
-- | Create a default frame and make it the top-level window.
frameCreateTopFrame :: String -> IO (Frame ())
frameCreateTopFrame title
  = do frame <- frameCreateDefault title
       frameSetTopFrame frame
       return frame

-- | Set the top-level frame (calls 'cAppSetTopWindow').
frameSetTopFrame :: Frame a -> IO ()
frameSetTopFrame frame
  = wxcAppSetTopWindow frame

-- | Create a  frame with default settings.
frameCreateDefault :: String -> IO (Frame ())
frameCreateDefault title
  = frameCreate objectNull idAny title rectNull frameDefaultStyle


-- | Center the frame on the screen.
frameCenter :: Frame a -> IO ()
frameCenter f 
  = frameCentre f wxBOTH

-- | Center the frame horizontally on the screen.
frameCenterHorizontal :: Frame a -> IO ()
frameCenterHorizontal f
  = frameCentre f wxHORIZONTAL

-- | Center the frame vertically on the screen.
frameCenterVertical :: Frame a -> IO ()
frameCenterVertical f
  = frameCentre f wxVERTICAL

------------------------------------------------------------------------------------------
-- Window
------------------------------------------------------------------------------------------
-- | The parent frame or dialog of a widget.
windowGetFrameParent :: Window a -> IO (Window ())
windowGetFrameParent w
  = if (instanceOf w classFrame || instanceOf w classDialog)
     then return (downcastWindow w)
     else do p <- windowGetParent w
             if (objectIsNull p)
              then return (downcastWindow w)
              else windowGetFrameParent p


-- | The ultimate root parent of the widget.
windowGetRootParent :: Window a -> IO (Window ())
windowGetRootParent w
  = do p <- windowGetParent w
       if (objectIsNull p)
        then return (downcastWindow w)
        else windowGetRootParent p
       


-- | Retrieve the current mouse position relative to the window position.
windowGetMousePosition :: Window a -> IO Point
windowGetMousePosition w
  = do p <- wxcGetMousePosition
       windowScreenToClient2 w p
  
-- | Get the window position relative to the origin of the display.
windowGetScreenPosition :: Window a -> IO Point
windowGetScreenPosition w
  = windowClientToScreen w pointZero


-- | Get the children of a window
windowChildren :: Window a -> IO [Window ()]
windowChildren w
  = do count <- windowGetChildren w ptrNull 0
       if count <= 0
        then return []
        else withArray (replicate count ptrNull) $ \ptrs ->
             do _  <- windowGetChildren w ptrs count
                ps <- peekArray count ptrs
                return (map objectFromPtr ps)

------------------------------------------------------------------------------------------
-- Statusbar
------------------------------------------------------------------------------------------
statusBarCreateFields :: Frame a -> [Int] -> IO (StatusBar ())
statusBarCreateFields parent widths
  = do pst <- windowGetWindowStyleFlag parent
       let st = if (bitsSet wxRESIZE_BORDER pst) then wxST_SIZEGRIP else 0
       sb <- frameCreateStatusBar parent (length widths) st
       let len = length widths
       if (len <= 1)
        then return sb
        else do withArray (map toCInt widths) (\pwidths -> statusBarSetStatusWidths sb (length widths) pwidths)
                return sb
