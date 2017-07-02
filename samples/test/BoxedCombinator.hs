module Main where
import Graphics.UI.WX

bugtext :: String
bugtext = unlines [ "Former bug: these buttons should react when clicked"
                  , "but the boxed one does not"
                  , ""
                  , "Buggy   in: MacOS X [now fixed!]"
                  , "Working in: Linux and Windows"
                  ]

main :: IO ()
main = start $ do
         f <- frame [ text := "program" ]
         b1 <- button f [ text := "click me" ]
         b2 <- button f [ text := "click me" ]
         b3 <- button f [ text := "click me" ]
         set b1 [ on command := set b1 [ text := "thanks!" ] ]
         set b2 [ on command := set b2 [ text := "thanks!" ] ]
         set b3 [ on command := set b3 [ text := "thanks!" ] ]
         set f [ visible := True
               , layout := margin 10 $ column 4
                  [ label bugtext, widget b1, (boxed "" $ column 2 [ (boxed "hey" $ widget b2)
                                                                   , widget b3 ]) ]
               ]
