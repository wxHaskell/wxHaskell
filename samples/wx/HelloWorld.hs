{- demonstrates the use of a simple menu, statusbar, and dialog -}
module Main where

import Graphics.UI.WX

main :: IO ()
main
  = start hello

hello :: IO ()
hello
  = do -- the application frame
       f      <- frame         [text := "Hello world!", clientSize := sz 300 200]                               

       -- create file menu  
       file   <- menuPane      [text := "&File"]
       _quit  <- menuQuit file [help := "Quit the demo", on command := close f]

       -- create Help menu
       hlp    <- menuHelp      []
       about  <- menuAbout hlp [help := "About wxHaskell"]

       -- create statusbar field
       status <- statusField   [text := "Welcome to wxHaskell"]

       -- set the statusbar and menubar
       set f [ statusBar := [status]
             , menuBar   := [file,hlp]
             -- as an example, put the menu event handler for an about box on the frame.
             ,on (menu about) := infoDialog f "About wxHaskell" "This is a wxHaskell demo"
             ]
