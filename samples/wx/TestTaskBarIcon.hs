module Main where

--

import Graphics.UI.WX
import Graphics.UI.WXCore
import System.Process

main = start gui

gui = do f <- frame [text := "Main Window"]
         icn <- iconCreateFromFile "../bitmaps/wxwin.ico" (Size 16 16)
         
         
         tbi <- taskBarIconCreate
         taskBarIconSetIcon tbi icn "Application Icon"
         evtHandlerOnTaskBarIconEvent tbi (onTaskBarEvt f tbi)
         
         btClose <- button f [text := "Close",
                              on command := do taskBarIconDelete tbi
                                               close f]
         set f [layout := margin 5 $
                          hfloatRight $ widget btClose 
               ]
         

onTaskBarEvt f tbi TaskBarIconRightDown =
         do
         popmenu <- menuPane []         
         m1 <- menuItem popmenu [text := "Show main Window", 
                                 on command := set f [visible := True]]
         m2 <- menuItem popmenu [text := "Quit",
                                 on command := infoDialog f "Dialog" "Quit pressed"] 
         taskBarIconPopupMenu tbi popmenu
         return ()
         
onTaskBarEvt _ _ _ = return ()
