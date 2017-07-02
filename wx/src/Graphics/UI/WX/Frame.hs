
--------------------------------------------------------------------------------
{-|
Module      :  Frame
Copyright   :  (c) Daan Leijen 2003
               (c) Jeremy O'Donoghue 2007
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Frames.
    
* Instances: 'Form', 'Closeable', 'Framed' --
             'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint',
             'HasDefault'.
             

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Frame
    ( -- * Frames
      Frame, frame, frameFixed, frameTool, frameEx
    , frameLoadRes, frameLoadChildRes
    , initialFrame
      -- * MDI Frames
    , MDIParentFrame, MDIChildFrame
    , mdiParentFrame, mdiChildFrame
    , mdiParentFrameEx, mdiChildFrameEx
     -- ** Operations
    , activeChild, activateNext, activatePrevious, arrangeIcons
    , cascade, tile
    -- * Internal
    ) where

import Prelude                hiding (id)
import Graphics.UI.WXCore     hiding (rect)

import Graphics.UI.WX.Types   hiding (rect)
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes hiding (parent, style)
import Graphics.UI.WX.Window
import Graphics.UI.WX.TopLevelWindow

defaultStyle :: Style
defaultStyle 
  = frameDefaultStyle -- .+. wxTAB_TRAVERSAL -- .+. wxNO_FULL_REPAINT_ON_RESIZE

-- | Create a top-level frame window.
frame :: [Prop (Frame ())]  -> IO (Frame ())
frame props
  = frameEx defaultStyle props objectNull

-- | Create a top-level frame window that is not resizeable.
frameFixed :: [Prop (Frame ())]  -> IO (Frame ())
frameFixed props
  = frameEx (defaultStyle .-. wxMAXIMIZE_BOX .-. wxRESIZE_BORDER)  props objectNull

-- | Create a tool window; floats on the parent and has a small caption.
frameTool :: [Prop (Frame ())]  -> Window a -> IO (Frame ())
frameTool props parent
  = frameEx (defaultStyle .-. wxFRAME_TOOL_WINDOW .-. wxFRAME_FLOAT_ON_PARENT)  props parent


-- | Create a top-level frame window in a custom style.
frameEx :: Style -> [Prop (Frame ())]  -> Window a -> IO (Frame ())
frameEx style_ props_ parent
  = feed2 props_ style_ $
    initialFrame $ \id rect txt -> \props' style' ->
    do f <- frameCreate parent id txt rect style'
       let initProps = (if (containsProperty visible props')
                        then [] else [visible := True]) ++
                       (if (containsProperty bgcolor props')
                        then [] else [bgcolor := colorSystem Color3DFace])
       set f initProps
       set f props'
       return f
     
-- | Complete the construction of a top level frame which has been loaded
--   from a resource file.
frameLoadRes :: FilePath -> String -> [Prop (Frame ())] -> IO (Frame ())
frameLoadRes rc name props = 
    frameLoadChildRes objectNull rc name props

-- | Complete the construction of a frame which is the child of some
--   existing parent window.
frameLoadChildRes :: Window a -> FilePath -> String -> [Prop (Frame ())] -> IO (Frame ())
frameLoadChildRes parent rc name props =
    do res <- xmlResourceCreateFromFile rc wxXRC_USE_LOCALE
       f   <- xmlResourceLoadFrame res parent name
       set f props
       return f

-- | initial Frame flags
initialFrame :: (Id -> Rect -> String -> [Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialFrame cont 
  = initialContainer    $ \id rect ->
    initialText         $ \txt ->
    initialResizeable   $
    initialMinimizeable $
    initialMaximizeable $
    initialCloseable    $
    initialClipChildren $
    initialFullRepaintOnResize $ 
    cont id rect txt 
             

{--------------------------------------------------------------------------
  MDI frames
--------------------------------------------------------------------------}
-- | Create an MDI parent frame.
mdiParentFrame :: [Prop (MDIParentFrame ())] -> IO (MDIParentFrame ())
mdiParentFrame props
  = mdiParentFrameEx objectNull defaultStyle props


-- | Create an MDI parent frame with a custom style.
mdiParentFrameEx :: Window a -> Style -> [Prop (MDIParentFrame ())] -> IO (MDIParentFrame ())
mdiParentFrameEx parent stl props
  = feed2 props stl $
    initialFrame $ \id rect txt -> \props' stl' ->
    do f <- mdiParentFrameCreate parent id txt rect stl'
       let initProps = (if (containsProperty visible props')
                        then [] else [visible := True]) ++
                       (if (containsProperty bgcolor props')
                        then [] else [bgcolor := colorSystem Color3DFace])
       set f initProps
       set f props'
       return f


-- | Create a MDI child frame.
mdiChildFrame :: MDIParentFrame a -> [Prop (MDIChildFrame ())] -> IO (MDIChildFrame ())
mdiChildFrame parent props
  = mdiChildFrameEx parent defaultStyle props

-- | Create a MDI child frame with a custom style.
mdiChildFrameEx :: MDIParentFrame a -> Style -> [Prop (MDIChildFrame ())] -> IO (MDIChildFrame ())
mdiChildFrameEx parent stl props
  = feed2 props stl $
    initialFrame $ \id rect txt -> \props' stl' ->
    do f <- mdiChildFrameCreate parent id txt rect stl'
       let initProps = (if (containsProperty visible props')
                         then [] else [visible := True]) ++
                       (if (containsProperty bgcolor props')
                         then [] else [bgcolor := colorSystem Color3DFace])
       set f initProps
       set f props'
       return f
  


-- | Return the active child frame ('objectIsNull' when no child is active)
activeChild :: ReadAttr (MDIParentFrame a) (MDIChildFrame ())
activeChild = readAttr "activeChild" mdiParentFrameGetActiveChild

-- | Activate the next child frame.
activateNext :: MDIParentFrame a -> IO ()
activateNext  = mdiParentFrameActivateNext

-- | Activate the previous child frame
activatePrevious :: MDIParentFrame a -> IO ()
activatePrevious  = mdiParentFrameActivatePrevious

-- | Arrange iconized mdi child frames.
arrangeIcons :: MDIParentFrame a -> IO ()
arrangeIcons = mdiParentFrameArrangeIcons

-- | Cascade the child frames.
cascade :: MDIParentFrame a -> IO ()
cascade = mdiParentFrameCascade 

-- | Tile the child frames
tile :: MDIParentFrame a -> IO ()
tile = mdiParentFrameTile


 
