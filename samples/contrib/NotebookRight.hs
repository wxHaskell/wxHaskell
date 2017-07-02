{-| 
    Author      :  Luc Taesch 2003
    Portability :  portable ? (tested on XP)
    
    Illustrate more controls from wxHaskell, Hacked from Controls.hs ( Daan Leijen 2003)
    namely bitmapButtons, righ click menus, vertical labels on notebooks, usage of tooltips
-}
 
{-# LANGUAGE FlexibleContexts      #-}

module Main where
 
import Graphics.UI.WX
import Graphics.UI.WXCore

import Paths_samplesContrib
 
main :: IO ()
main
  = start gui
 
gui :: IO ()
gui
  = do -- main gui elements: frame, panel, text control, and the notebook
       f       <- frame [text := "Controls" ]
       p       <- panel f []
       
       -- use text control as logger
       textlog <- textCtrl p [wrap := WrapLine, enabled := False]
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              
       
       -- menu
       file         <- menuPane      [text := "&File"] 
       aRightClick  <- menuItem file [text := "Say Something\tCtrl+Q", help := "An interesting Message"] 
  
       -- button page
       nb   <- notebookRight p []
       p1   <- panel  nb []
       img  <- getDataFileName "bitmaps/wxwin16.png"
       ok   <- bitmapButton p1 [picture := img,
                                text := "Ok", on command := logMessage "bitmap button pressed", 
                                tooltip := "tooltip",
                                on clickRight := (\pt_ -> menuPopup file pt_ p)]
 
 
       -- specify layout
       set f [menuBar := [file]
             ,layout :=
                container p $
                column 0
                 [ tabs nb
                    [tab "buttons" (container p1 $ margin 10 $ floatCentre $ row 5 $ [widget ok])
                    ]
                 , hfill $ widget textlog                 
                 ]
                 , on (menu aRightClick) := infoDialog f "Say.." "Something"
             , clientSize := sz 400 300 ]
       return ()
{- 
  where
    logSelect labels w
      = do i <- get w selection
           s <- get w (item i)
           logMessage ("selected index: " ++ show i ++ ": " ++ s)
-}


-- like notebook, with labels created on the side ( rather than on top): wxNB_RIGHT
notebookRight :: Window a
                 -> [Prop (Notebook ())] -> IO (Notebook ())
notebookRight parent_ props 
  = do nb <- notebookCreate parent_ idAny rectNull ( wxCLIP_CHILDREN + wxNB_RIGHT)
       set nb props
       return nb
