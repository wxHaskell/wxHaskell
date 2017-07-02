{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
{-|
Module      :  TopLevelWindow
Copyright   :  (c) Jeremy O'Donoghue, 2007
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

wxTopLevelwindow (wxWidgets >= 2.8.0) defines an (abstract) common base class
for wxFrame and wxDialog.

In the wxHaskell implementation, TopLevel has been added to encapsulate 
some of the common functionality between the 'Dialog' and 'Frame' modules.
         
* Instances: 'HasDefault', 'Pictured', 'Framed', 'Form', 'Closeable'
* Instances inherited from 'Window': 'Textual', 'Literate', 'Dimensions', 
             'Colored', 'Visible', 'Child', 'Able', 'Tipped', 'Identity', 
             'Styled', 'Reactive', 'Paint'.   

-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.TopLevelWindow
    (   initialResizeable
      , initialMaximizeable 
      , initialMinimizeable
      , initialCloseable
    ) where

import Graphics.UI.WXCore hiding (win)

import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Window() -- Import instances alone

-- The default control activated by return key
instance HasDefault (TopLevelWindow a) where
  unsafeDefaultItem = newAttr "unsafeDefaultItem" getter setter
    where
      getter :: (TopLevelWindow a) -> IO (Window ())
      getter tlw = topLevelWindowGetDefaultItem tlw
      setter tlw win = topLevelWindowSetDefaultItem tlw win
  defaultButton = newAttr "defaultButton" getter setter
    where
      getter :: (TopLevelWindow a) -> IO (Button ())
      getter tlw = topLevelWindowGetDefaultButton tlw
      setter tlw win = topLevelWindowSetDefaultButton tlw win

-- The icon of a frame. 
instance Pictured (TopLevelWindow a) where
  picture = writeAttr "picture"  topLevelWindowSetIconFromFile

-- Defaults for framed TopLevel windows
instance Framed (TopLevelWindow a) where
  resizeable   = windowResizeable
  maximizeable = windowMaximizeable
  minimizeable = windowMinimizeable
  closeable    = windowCloseable

-- Default layout implementation
instance Form (Frame a) where
  layout = writeAttr "layout" windowSetLayout

-- Default window close
instance Closeable (Frame a) where
  close f
    = unitIO (windowClose f True {- force? -})

{--------------------------------------------------------------------------
  Framed instances
--------------------------------------------------------------------------}
-- | Display a resize border on a 'Frame' or 'Dialog' window.  Also enables or
-- disables the the maximize box.
-- This attribute must be set at creation time.
windowResizeable :: CreateAttr (Window a) Bool
windowResizeable
  = reflectiveAttr "resizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxRESIZE_BORDER s)
    setFlag w resize
      = set w [style :~ \stl -> if resize 
                                 then stl .+. wxRESIZE_BORDER .+. wxMAXIMIZE_BOX
                                 else stl .-. wxRESIZE_BORDER .-. wxMAXIMIZE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'windowResizable' flag in of the properties
initialResizeable :: ([Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialResizeable 
  = withStyleProperty windowResizeable (wxRESIZE_BORDER .+. wxMAXIMIZE_BOX) 


-- | Display a maximize box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowMaximizeable :: CreateAttr (Window a) Bool
windowMaximizeable
  = reflectiveAttr "maximizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxMAXIMIZE_BOX s)
    setFlag w max_
      = set w [style :~ \stl -> if max_ then stl .+. wxMAXIMIZE_BOX else stl .-. wxMAXIMIZE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'windowMaximizable' flag in of the properties
initialMaximizeable :: ([Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialMaximizeable 
  = withStyleProperty windowMaximizeable wxMAXIMIZE_BOX 


-- | Display a minimize box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowMinimizeable :: CreateAttr (Window a) Bool
windowMinimizeable
  = reflectiveAttr "minimizeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxMINIMIZE_BOX s)
    setFlag w min_
      = set w [style :~ \stl -> if min_ then stl .+. wxMINIMIZE_BOX else stl .-. wxMINIMIZE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'windowMinimizable' flag in of the properties
initialMinimizeable :: ([Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialMinimizeable 
  = withStyleProperty windowMinimizeable wxMINIMIZE_BOX 


-- | Display a close box on a 'Frame' or 'Dialog' window.
-- This attribute must be set at creation time.
windowCloseable :: CreateAttr (Window a) Bool
windowCloseable
  = reflectiveAttr "closeable" getFlag setFlag
  where
    getFlag w
      = do s <- get w style
           return (bitsSet wxCLOSE_BOX s)
    setFlag w min_
      = set w [style :~ \stl -> if min_ then stl .+. wxCLOSE_BOX else stl .-. wxCLOSE_BOX]

-- | Helper function that transforms the style accordding
-- to the 'windowMinimizable' flag in of the properties
initialCloseable :: ([Prop (Window w)] -> Style -> a) -> [Prop (Window w)] -> Style -> a
initialCloseable 
  = withStyleProperty windowCloseable wxCLOSE_BOX
