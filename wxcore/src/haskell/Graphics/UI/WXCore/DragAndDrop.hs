-----------------------------------------------------------------------------------------
{-|
Module      :  DragAndDrop
Copyright   :  (c) shelarcy 2007
License     :  wxWidgets

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Drag & Drop events.
-}
-----------------------------------------------------------------------------------------

module Graphics.UI.WXCore.DragAndDrop (
        -- * Drop Targets
          dropTarget

        -- * Create Drop Soruces
        , dropSource
        , dropSourceWithCursor
        , dropSourceWithCursorByString
        , dropSourceWithIcon
        ) where

import Graphics.UI.WXCore.Defines
import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.WxcObject
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcClassTypes
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassesMZ

import Foreign.Ptr


{--------------------------------------------------------------------------------
  Drop Target
--------------------------------------------------------------------------------}
-- | Set a drop target window and 'DataObject' that is associated with drop event.
dropTarget :: Window a -> DataObject b -> IO (WXCDropTarget  ())
dropTarget window wxdata = do
    drop' <- wxcDropTargetCreate nullPtr
    dropTargetSetDataObject drop' wxdata
    windowSetDropTarget window drop'
    return drop'

-- | Create 'DropSource'. Then 'dragAndDrop' replace target\'s 'DataObject' by this 'DataObject'.
dropSource :: DataObject a -> Window b -> IO (DropSource ())
dropSource wxdata win' =
    withObjectPtr nullIcon $ \iconCopy ->
    withObjectPtr nullIcon $ \iconMove ->
    withObjectPtr nullIcon $ \iconNone ->
    dropSourceCreate wxdata win' iconCopy iconMove iconNone

-- | On Windows or Mac OS X platform, you must choose this function or 'dropSourceWithCursorByString',
-- if you want to use Custom Cursor for Drag & Drop event. 'dropSourceWithIcon' doesn't work these
-- platform, and this function doesn't work other platforms.
dropSourceWithCursor :: DataObject a -> Window b -> Cursor c -> Cursor c -> Cursor c -> IO (DropSource ())
dropSourceWithCursor wxdata win' copy move none =
    withObjectPtr copy $ \dndCopy ->
    withObjectPtr move $ \dndMove ->
    withObjectPtr none $ \dndNone ->
    dropSourceCreate wxdata win' dndCopy dndMove dndNone

dropSourceWithIcon :: DataObject a -> Window b -> Icon c -> Icon c -> Icon c -> IO (DropSource ())
dropSourceWithIcon wxdata win' copy move none =
    withObjectPtr copy $ \dndCopy ->
    withObjectPtr move $ \dndMove ->
    withObjectPtr none $ \dndNone ->
    dropSourceCreate wxdata win' dndCopy dndMove dndNone

dropSourceWithCursorByString :: DataObject a -> Window b -> String -> String -> String -> IO (DropSource ())
dropSourceWithCursorByString wxdata win' copy move none =
   case wxToolkit of
     WxMSW -> do
         dndCopy <- cursorCreateFromFile copy
         dndMove <- cursorCreateFromFile move
         dndNone <- cursorCreateFromFile none
         dropSourceWithCursor wxdata win' dndCopy dndMove dndNone
     WxMac -> do
         dndCopy <- cursorCreateFromFile copy
         dndMove <- cursorCreateFromFile move
         dndNone <- cursorCreateFromFile none
         dropSourceWithCursor wxdata win' dndCopy dndMove dndNone
     _ -> do
         dndCopy <- iconCreateFromFile copy sizeNull
         dndMove <- iconCreateFromFile move sizeNull
         dndNone <- iconCreateFromFile none sizeNull
         dropSourceWithIcon wxdata win' dndCopy dndMove dndNone

