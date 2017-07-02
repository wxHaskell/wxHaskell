module Main where

import Graphics.UI.WXCore
import Graphics.UI.WX

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do f  <- frame [text := "Paint demo", fullRepaintOnResize := False]
       sw <- scrolledWindow f [ on paint := onpaint
                              , virtualSize := sz 500 500, scrollRate := sz 10 10
                              , fullRepaintOnResize := False]
       set f [clientSize := sz 150 150, layout := fill $ widget sw]
       return ()
  where
    onpaint dc _viewArea
      = do circle dc (pt 200 200) 20 [penKind := PenDash DashDot]
           arc dc (pt 100 100) 20 90 230 [color := red, penWidth :~ (+1), penKind := PenSolid]
           ellipticArc dc (rect  (pt 20  20) (sz 60 30)) 90 230 [color := blue, penWidth :~ (*2)]
           c <- get dc color
           -- set dc [font := fontDefault{ fontFace = "Courier New", fontSize = 16, fontWeight = WeightBold }]
           set dc [fontFace := "Courier New", fontSize := 16, fontWeight := WeightBold ]
           drawText dc (show c) (pt 50 50) []
           rotatedText dc "rotated text" (pt 80 160) 45 [textColor := green]
