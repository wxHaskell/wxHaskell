--------------------------------------------------------------------------------
{-|
Module      :  Variable
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Mutable variables.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Variable
            ( variable
            ) where

import Control.Concurrent.STM.TVar
import Graphics.UI.WX.Types( Var, varGet, varSet, varCreate, varUpdate )
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes

{--------------------------------------------------------------------

--------------------------------------------------------------------}
instance Valued TVar where
  value = makeAttr "value" varGet varSet varUpdate

-- | Create a mutable variable. Change the value using the |value| attribute.
variable :: [Prop (Var a)] -> IO (Var a)
variable props
  = do v <- varCreate (error "Graphics.UI.WX.Variable: uninitialized variable, use the 'value' attribute at creation")
       set v props
       return v
