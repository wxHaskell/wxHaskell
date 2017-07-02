module Main where

--

import Graphics.UI.WX
import Graphics.UI.WXCore

import Paths_samplesWx


main :: IO ()
main = start gui

gui :: IO ()
gui = do f <- frame [text := "Main Window"]
         icf <- getDataFileName "bitmaps/wxwin.ico"
         icn <- iconCreateFromFile icf (Size 16 16)
         
         tbi <- taskBarIconCreate
         _   <- taskBarIconSetIcon tbi icn "Application Icon"
         evtHandlerOnTaskBarIconEvent tbi (onTaskBarEvt f tbi)
          
         btClose <- button f [text := "Close",
                              on command := do taskBarIconDelete tbi
                                               close f]
         set f [layout := margin 5 $
                          hfloatRight $ widget btClose 
               ]


onTaskBarEvt :: Window a         ->
                TaskBarIcon b    ->
                EventTaskBarIcon ->
                IO ()
onTaskBarEvt f tbi TaskBarIconRightDown =
      do
         popmenu <- menuPane []         
         _m1 <- menuItem popmenu [text := "Show main Window", 
                                  on command := set f [visible := True]]
         _m2 <- menuItem popmenu [text := "Quit",
                                  on command := infoDialog f "Dialog" "Quit pressed"] 
         _   <- taskBarIconPopupMenu tbi popmenu
         return ()
         
onTaskBarEvt _ _ _ = return ()
