
import Graphics.UI.WX
import Graphics.UI.WXCore

main :: IO ()
main = start $ do
         f <- frame [text := "Scintilla Test"]
         s <- styledTextCtrlCreate f 0 "bla" (Rect 0 0 500 500) 0
         styledTextCtrlInsertText s 0 "hello world!"
         return ()
