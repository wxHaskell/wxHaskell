{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
--------------------------------------------------------------------------------
{-|
Module      :  Timer
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Support for millisecond timers.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Timer
            ( Timer, timer, interval
            ) where

import Graphics.UI.WXCore.WxcClasses hiding (Timer)
import Graphics.UI.WXCore.Events  

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events

import Control.Monad (void)


{--------------------------------------------------------------------

--------------------------------------------------------------------}
-- | A timer generates a 'command' event on a specified millisecond 'interval'.
--
-- * Attributes: 'interval'
--
-- * Instances: 'Able', 'Commanding'
--
type Timer  = TimerEx ()

-- | Create a new timer with a 1 second interval. The timer is automatically discarded
-- when the parent is deleted.
timer :: Window a -> [Prop Timer] -> IO Timer
timer parent' props
  = do t <- windowTimerCreate parent'
       void (timerStart t 1000 False)
       set t props
       return t

-- | The millisecond interval of the timer.
interval :: Attr Timer Int
interval
  = newAttr "timer-interval"
      (\t   -> timerGetInterval t)
      (\t i -> do runs <- timerIsRuning t
                  if (runs)
                   then do timerStop t
                           isone <- timerIsOneShot t
                           void $ timerStart t i isone
                   else do void $ timerStart t i True
                           timerStop t)

instance Able Timer where
  enabled
    = newAttr "enabled"
        (\t      -> timerIsRuning t)
        (\t able -> do runs <- timerIsRuning t
                       when (runs /= able)
                        (if able then do i <- get t interval
                                         void $ timerStart t i False
                                 else do timerStop t))

instance Commanding Timer where
  command
    = newEvent "command" timerGetOnCommand timerOnCommand
