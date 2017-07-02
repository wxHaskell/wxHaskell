{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
{-|
Module      :  Dialogs
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Defines common dialogs.

* Instances: 'Form', 'Framed' -- 
             'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Dialogs
    ( -- * Generic dialogs
      Dialog, dialog, showModal
     -- * Messages
    , errorDialog, warningDialog, infoDialog
    , confirmDialog, proceedDialog
      -- * Files
    , fileOpenDialog, filesOpenDialog
    , fileSaveDialog
    , dirOpenDialog
      -- * Misc.
    , fontDialog
    , colorDialog
    , passwordDialog
    , textDialog
    , numberDialog
      -- * Primitive
    , dialogEx
    ) where

import Graphics.UI.WXCore

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Events()  -- just for haddock
import Graphics.UI.WX.Frame  

instance Form (Dialog a) where
  layout
    = writeAttr "layout" windowSetLayout

-- | Create a dialog window. Use 'showModal' for a modal dialog. Use
-- the 'visible' property to show\/hide a modeless dialog.
dialog :: Window a -> [Prop (Dialog ())] -> IO (Dialog ())
dialog parent props
  = dialogEx parent (dialogDefaultStyle {- .+. wxNO_FULL_REPAINT_ON_RESIZE -}) props

-- | Create a dialog window with a certain style.
dialogEx :: Window a -> Style -> [Prop (Dialog ())] -> IO (Dialog ())
dialogEx parent style props
  = feed2 props style $
    initialFrame $ \id_ rect_ txt -> \props_ flags -> 
    do d <- dialogCreate parent id_ txt rect_ flags
       set d props_
       return d

-- | Show a modal dialog. Take a function as argument that takes a function itself
-- as argument that can be used to end the modal dialog. The argument of this function
-- is returned as the result of the dialog. The result is 'Nothing' when the dialog
-- is dismissed via the system menu.
--
-- > d   <- dialog w [text := "Demo"]
-- > ok  <- button d [text := "Ok"]
-- > ...
-- > result <- showModal d (\stop -> set ok [on command := stop (Just 42)])
--
showModal :: Dialog b -> ((Maybe a -> IO ()) -> IO ()) -> IO (Maybe a)
showModal dialog_ f
  = do ret <- varCreate Nothing
       f (\x -> do{ varSet ret x; dialogEndModal dialog_ 0} )
       _ <- dialogShowModal dialog_
       varGet ret
