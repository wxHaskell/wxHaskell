{- From Maarten <maarten <at> snowlion.nl>
-
The following code doesn't seem to work properly. Either the main entry
(m1/mp1) or it's sub menu entry (ms1/mps1) do not seem to propagate the
event when pressed. It is possible to make it working by uncommenting the
lines where the menu commands are registered in the frame.
-}

module Main where

import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  f <- frame [ text := "Hello world!" ]

  m   <- menuPane [ text := "&Menu" ]
  m1 <- menuItem m [ text := "Menu m1"
                   , on command := putStrLn "menu m1"]
  --  set f [ on (menu m1) := putStrLn "menu m1" ]
  menuLine m
  sub <- menuPane [text := "Sub menu"]
  ms1 <- menuItem sub [ text := "submenu ms1"
                      , on command := putStrLn "submenu ms1" ]
  --  set f [ on (menu ms1) := putStrLn "submenu ms1" ]
  menuSub m sub [ text := "Sub" ]
  menuItem m [text := "E&xit", on command := close f]

  set f [menuBar := [m], on mouse := mouseEvent f, clientSize := sz 200 200 ]
  return ()

mouseEvent f eventMouse = do
  case eventMouse of
    MouseRightDown mousePoint _ -> doPopup f mousePoint
    _ -> return ()

doPopup f mousePoint = do
  m <- makePopupMenu f "&Popup" "Doesnt' work..."
  menuPopup m mousePoint f
  objectDelete m

makePopupMenu f c t = do
  mp   <- menuPane [ text := c ]
  mp1 <- menuItem mp [ text := "Popup mp1"
                     , on command := putStrLn "popup mp1"]
  --  set f [ on (menu mp1) := putStrLn "popup mp1" ]
  menuLine mp
  sub <- menuPane [text := "more text"]
  mps1 <- menuItem sub [ text := "Popup mps1"
                       , on command := putStrLn "popup mps1"]
  menuSub mp sub [ text := "Sub" ]
  --  set f [ on (menu mps1) := putStrLn "popup mps1" ]
  return mp
