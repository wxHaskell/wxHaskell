-- Printing demo.
module Main where

import Graphics.UI.WXCore.Print
import Graphics.UI.WX

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do -- the application frame
       f         <- frame [text := "Print demo", clientSize := sz 300 200]                               

       -- the page setup dialog remembers application settings: margins, page size, etc.
       pageSetup <- pageSetupDialog f 25 {- initial margin in mm -}
  
       -- create file menu  
       file     <- menuPane [text := "&File"]

       _mprint  <- menuItem file 
                    [ text := "&Print..."
                    , help := "Print a test"
                    , on command := printDialog pageSetup "Test"  pageFun printFun
                    ]
       _mpreview <- menuItem file 
                    [ text := "P&rint preview"
                    , help := "Print preview"
                    , on command := printPreview pageSetup "Test" pageFun printFun 
                    ]
       _msetup  <- menuItem file
                    [ text := "Page &setup...", help := "Setup the page"
                    , on command := pageSetupShowModal pageSetup
                    ]
       _quit    <- menuQuit file [help := "Quit the demo", on command := close f]

       -- create statusbar field
       status <- statusField   [text := "Welcome to the wxHaskell print demo"]

       -- set the statusbar and menubar
       set f [ statusBar := [status]
             , menuBar   := [file]
             ]

  where
     -- Print a page
     printFun :: PrintFunction
     printFun _pageInfo _printInfo printSize dc nr 
        = do set dc [brush := brushTransparent]
             drawText dc ("Page " ++ show nr) (pt 0 0) [fontFamily := FontRoman, fontSize := 14]
             let area_ = rectFromSize printSize
                 mid   = rectCentralPoint area_
             drawRect dc area_ [color := grey]  -- draw a border around the printable area
             circle dc mid 4  [color := red]   -- draw a center bullet
              
     -- Return the page range 
     pageFun :: PageFunction
     pageFun _pageInfo _printInfo _printSize 
        = (2,5) 
