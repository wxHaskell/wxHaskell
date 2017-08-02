
-----------------------------------------------------------------------------------------
{-|
Module      :  Layout
Copyright   :  (c) Daan Leijen 2003
License     :  BSD-style

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Just re-exports functionality of "Graphics.UI.WXCore.Layout". See that module
for a description of layout combinators. 

Any object in the 'Form' class has a 'layout' attribute to specify the 
layout. Here is a short example:

> do f <- frame [text := "layout demo"]
>    q <- button f [text := "quit", on command := close f]
>    set f [layout := margin 10 $
>                     floatCentre $
>                     column 5 [label "hi",widget q]]

-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WX.Layout
             (
             -- * Classes
               Form, layout
             -- * Layout
             , module Graphics.UI.WXCore.Layout
             ) where

import Graphics.UI.WXCore.Layout
import Graphics.UI.WX.Attributes

{--------------------------------------------------------------------

--------------------------------------------------------------------}
-- | Forms can contain other widgets.
class Form w where
  -- | Set the layout of the child widgets.
  layout :: Attr w Layout
