--------------------------------------------------------------------------------
{-| 
Module      :  WX
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

The WX module just re-exports functionality from helper modules and
defines the 'start' function.

The WX library provides a /haskellized/ interface to the raw wxWidgets
functionality provided by the "Graphics.UI.WXCore" library.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX
  ( -- * Functions
    start
    -- * Modules
  , module Graphics.UI.WX.Types
  , module Graphics.UI.WX.Attributes
  , module Graphics.UI.WX.Classes
  , module Graphics.UI.WX.Variable
  , module Graphics.UI.WX.Layout
  , module Graphics.UI.WX.Events

  , module Graphics.UI.WX.Window
  , module Graphics.UI.WX.Frame
  , module Graphics.UI.WX.Timer
  , module Graphics.UI.WX.Media
  , module Graphics.UI.WX.Menu
  , module Graphics.UI.WX.Controls
  , module Graphics.UI.WX.Dialogs
  , module Graphics.UI.WX.Draw
  ) where

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Variable
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Events

import Graphics.UI.WX.Window
import Graphics.UI.WX.Frame
import Graphics.UI.WX.Timer
import Graphics.UI.WX.Media
import Graphics.UI.WX.Menu

import Graphics.UI.WX.Controls
import Graphics.UI.WX.Dialogs
import Graphics.UI.WX.Draw

import Graphics.UI.WXCore

-- | 'start' runs the GUI.
start :: IO a -> IO ()
start io
  = run (unitIO io)
