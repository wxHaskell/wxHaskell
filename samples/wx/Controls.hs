{--------------------------------------------------------------------------------
 Copyright (c) Daan Leijen 2003
 wxWindows License.

 Demonstrates: 
 - many different kind of controls
 - message logging.
--------------------------------------------------------------------------------}

{-# LANGUAGE FlexibleContexts      #-}

module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore 

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do -- main gui elements: frame, panel, text control, and the notebook
       f       <- frame [text := "Controls"]
       p       <- panel f []
       nb      <- notebook p []
       textlog <- textCtrl p [enabled := False, wrap := WrapNone] 

       -- use text control as logger
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              
       -- set f [on closing :~ \prev -> do logSetActiveTarget oldlog; logDelete log; prev]

       -- button page
       p1   <- panel  nb []
       ok   <- button p1 [text := "Ok", on command := logMessage "ok button pressed"]
       quit <- button p1 [text := "Quit", on command := close f]

       -- radio box page
       p2   <- panel  nb []
       let rlabels = ["first", "second", "third"]
       r1   <- radioBox p2 Vertical rlabels   [text := "radio box", on select ::= logSelect]
       r2   <- radioBox p2 Horizontal rlabels [tooltip := "radio group two", on select ::= logSelect]
       rb1  <- button   p2 [text := "disable", on command ::= onEnable r1]

       -- choice
       p3   <- panel nb []
       let clabels = ["mies","noot","aap"]
       c1   <- choice p3 [tooltip := "unsorted choices", on select ::= logSelect, sorted  := False, items := clabels]
       c2   <- choice p3 [tooltip := "sorted choices", on select ::= logSelect, sorted  := True, items := clabels] 
       cb1  <- button p3 [text := "disable", on command ::= onEnable c1]

       -- list box page
       p4   <- panel nb []
       sl1  <- singleListBox p4 
                  [items      := clabels
                  ,tooltip    := "unsorted single-selection listbox"
                  ,on select ::= logSelect]
       sl2  <- singleListBox p4 
                  [items      := clabels
                  ,tooltip    := "sorted listbox"
                  ,on select ::= logSelect, sorted     := True]
       sc1  <- checkBox p4 [text := "enable the listbox", checked := True, on command := set sl1 [enabled :~ not]]

       -- slider/gauge page
       p5   <- panel nb []
       s    <- hslider p5 True {- show labels -} 1 100 [selection := 50]
       g    <- hgauge  p5 100 [selection := 50]
       set s [on command := do{ i <- get s selection; set g [selection := i]} ]

       -- specify layout
       set f [layout :=
                container p $
                column 0
                 [ tabs nb
                    [tab "buttons" $ 
                     container p1 $ margin 10 $ floatCentre $ row 5 [widget ok, widget quit]
                    ,tab "radio box" $ 
                     container p2 $ margin 10 $ column 5 [ hstretch $ widget rb1
                                                         , row 0 [floatLeft $ widget r1
                                                                 ,floatRight $ widget r2]]
                    ,tab "choice" $ 
                     container p3 $ margin 10 $ column 5 [ hstretch $ widget cb1
                                                         , row 0 [floatLeft $ widget c1
                                                                 ,floatRight $ row 5 [label "sorted: ", widget c2]]]
                    ,tab "listbox" $ 
                     container p4 $ margin 10 $ column 5 [ hstretch  $ dynamic $ widget sc1
                                                         , floatLeft $
                                                           row 0 [widget sl1, widget sl2]]
                    ,tab "slider" $ 
                     container p5 $ margin 10 $ column 5 [ hfill $ widget s
                                                         , hfill $ widget g
                                                         , glue
                                                         ]
                    ]
                 , hfill $ minsize (sz 20 80) $ widget textlog
                 ]
             , clientSize := sz 400 300 ]
       return ()

  where
    -- logSelect :: (Selection w, Items w String) => w -> IO ()
    logSelect w
      = do i <- get w selection
           s <- get w (item i)
           logMessage ("selected index: " ++ show i ++ ": " ++ s)
           

    onEnable w b
      = do set w [enabled :~ not]
           enable <- get w enabled
           set b [text := (if enable then "disable" else "enable")]

kindof :: WxObject a -> String -> IO ()
kindof obj className
  = do classInfo_ <- classInfoFindClass className
       if (objectIsNull classInfo_)
        then logMessage ("kindof " ++ className ++ ": no such class")
        else if (objectIsNull obj)
              then logMessage ("kindof " ++ className ++ ": null object")
              else do haskind <- objectIsKindOf obj classInfo_
                      logMessage ("kindof " ++ className ++ ": " ++ show haskind)
