module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Paths_samplesTest

colorscheme = [ ( wxSTC_HA_DEFAULT,       rgb 0   0   0   )
              , ( wxSTC_HA_IDENTIFIER,    rgb 0   0   0   )
              , ( wxSTC_HA_KEYWORD,       rgb 0   0   255 )
              , ( wxSTC_HA_NUMBER,        rgb 100 100 100   )
              , ( wxSTC_HA_STRING,        rgb 100 100 200 )
              , ( wxSTC_HA_CHARACTER,     rgb 0   100 200 )
              , ( wxSTC_HA_CLASS,         rgb 255 0   255 )
              , ( wxSTC_HA_MODULE,        rgb 255 0   0   )
              , ( wxSTC_HA_CAPITAL,       rgb 0   255 0   )
              , ( wxSTC_HA_DATA,          rgb 255 0   0   )
              , ( wxSTC_HA_IMPORT,        rgb 150 0   200 )
              , ( wxSTC_HA_OPERATOR,      rgb 256 0   0   )
              , ( wxSTC_HA_INSTANCE,      rgb 150 61  90  )
              , ( wxSTC_HA_COMMENTLINE,   rgb 10  80  100 )
              , ( wxSTC_HA_COMMENTBLOCK,  rgb 0   60  0   )
              , ( wxSTC_HA_COMMENTBLOCK2, rgb 0   30  0   )
              , ( wxSTC_HA_COMMENTBLOCK3, rgb 0   10  0   )
              ]

keywords = "as case class data default deriving do else hiding if import " ++
           "in infix infixl infixr instance let module newtype of qualified" ++
           "then type where"

main = start $ do
    f <- frame [text := "Scintilla Test", visible := False]
    p <- panel f []
    s <- styledTextCtrl p [ clientSize := sz 500 500]
    filePath <- getDataFileName "STCLexer.hs"
    styledTextCtrlLoadFile s filePath
    styledTextCtrlStyleClearAll s
    styledTextCtrlSetLexer s wxSTC_LEX_HASKELL
    styledTextCtrlSetKeyWords s 0 keywords
    let fontstyle = fontFixed { _fontFace = "Monospace" }
    (font, _) <- fontCreateFromStyle fontstyle
    mapM_ (\style -> styledTextCtrlStyleSetFont s style font) [0..wxSTC_STYLE_LASTPREDEFINED]
    sequence_ [styledTextCtrlStyleSetForeground s k c | (k, c) <- colorscheme]
    set f [ layout := container p $ fill $ widget s ]
    set f [ visible := True ]
