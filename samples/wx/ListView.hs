{--------------------------------------------------------------------------------
   List view demo.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX

entries :: [Int]
entries = [-1,1,-123,123]

main :: IO ()
main = start gui

gui :: IO ()
gui = do 
  f <- frame [text := "List view Sample"]
  p <- panel f []
  
  l  <- listView p ["n", "n >= 0"] (\n-> [show n, show (n >= 0)]) 
  listViewSetItems l entries
  
  -- specify layout
  set f [layout := container p . fill . widget . listViewCtrl $ l, clientSize := sz 230 400]
  return ()
  
