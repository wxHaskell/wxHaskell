-- Compile with
--   ghc -optl-s --make NonModalDialog.hs

module Main where
import Graphics.UI.WX
main = start $ do
         f <- frame [ text := "program" ]
         w <- get f parent    -- added line
         d <- dialog w [ text := "Some dialog" ]
         let bugtext = unlines [ "Bug: closing these two windows should exit the"
                               , "program, but it does not" ]
         set d [ visible := True, layout := margin 10 $ label bugtext ]
