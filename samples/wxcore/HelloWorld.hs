{--------------------------------------------------------------------------------
  The 'hello world' demo from the wxWidgets site.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WXCore

main :: IO ()
main
  = run helloWorld

helloWorld :: IO ()
helloWorld
  = do -- create file menu
       fm <- menuCreate "" 0
       menuAppend fm wxID_ABOUT "&About.." "About wxHaskell" False {- not checkable -}
       menuAppendSeparator fm
       menuAppend fm wxID_EXIT "&Quit\tCtrl-Q"    "Quit the demo"  False

       -- create menu bar
       m  <- menuBarCreate 0
       _  <- menuBarAppend m fm "&File"

       -- create top frame
       f  <- frameCreate objectNull idAny "Hello world" rectZero frameDefaultStyle
       _  <- windowSetBackgroundColour f white
       windowSetClientSize f (sz 600 250)

       -- set status bar with 1 field
       _  <- frameCreateStatusBar f 1 0
       frameSetStatusText f "Welcome to wxHaskell" 0

       -- connect menu
       frameSetMenuBar f m
       evtHandlerOnMenuCommand f wxID_ABOUT (onAbout f)
       evtHandlerOnMenuCommand f wxID_EXIT  (onQuit f)

       -- show it
       _  <- windowShow f
       windowRaise f
       return ()
  where
    onAbout f
      = do version <- versionNumber
           _ <- messageDialog f "About 'Hello World'" ("This is a wxHaskell " ++ show version ++ " sample") (wxOK + wxICON_INFORMATION)
           return ()

    onQuit f
      = do _ <- windowClose f True {- force close -}
           return ()
