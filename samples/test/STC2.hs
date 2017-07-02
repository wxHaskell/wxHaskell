
import Graphics.UI.WX
import Graphics.UI.WXCore

main :: IO ()
main = start $ do
    f <- frame [text := "Scintilla Test"]
    p <- panel f []
    s <- styledTextCtrlCreate p 0 "bla" (Rect 0 0 500 500) 0
    b <- button p [text:= "print text in console", 
                   on command := styledTextCtrlGetText s >>= putStrLn]
    set f [ layout := container p $ column 5 [ fill $ widget s, 
            hfill $ widget b], clientSize := sz 300 300]
    return ()
