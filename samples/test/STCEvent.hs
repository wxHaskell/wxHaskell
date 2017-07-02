
import Graphics.UI.WX
import Graphics.UI.WXCore

calltiptext :: String
calltiptext = "I can write whatever I want here"

main :: IO ()
main = start $ do
         f <- frame [text := "Scintilla Test"]
         p <- panel f []
         textlog <- textCtrl p [clientSize := sz 500 200]
         textCtrlMakeLogActiveTarget textlog
         logMessage "logging enabled"
         s <- styledTextCtrl p []
         set s [on stcEvent := handler s]
         styledTextCtrlSetMouseDwellTime s 2000
         set f [ layout := container p $
                           column 5 $ [ fill $ widget s
                                      , hfill $ widget textlog
                                      ]
               , clientSize := sz 500 500
               ]

handler :: StyledTextCtrl a -> EventSTC -> IO ()
handler _ STCUpdateUI = return ()
handler _ STCStyleNeeded = return ()
handler _ STCPainted = return ()
handler stc e = do logMessage $ show e
                   case e of
                    (STCDwellStart xy) -> do
                      pos <- styledTextCtrlPositionFromPoint stc xy
                      styledTextCtrlCallTipShow stc pos calltiptext
                    (STCDwellEnd _xy) -> do
                      active <- styledTextCtrlCallTipActive stc
                      when active $ styledTextCtrlCallTipCancel stc
                    _ -> return ()
