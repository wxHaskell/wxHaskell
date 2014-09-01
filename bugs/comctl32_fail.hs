-- Bug 2810904

-- module Main () where

import Graphics.UI.WX
import Graphics.UI.WXCore

main = start mainFrame

mainFrame = 
    do
      f <- frame [text := "test"]
      button f []
      -- supposed to crash unless notebook below is present.
      -- notebook f [visible := False]
