module Main where

import Graphics.UI.WXCore 
import Graphics.UI.WX

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do f      <- frame [text := "Process test"]
       p      <- panel f []                       -- panel for tab-management etc.
       input  <- comboBox p [processEnter := True, text := "cmd"]
       output <- textCtrlRich p [bgcolor := black, textColor := red, font := fontFixed{ _fontSize = 12 }]
       stop_  <- button p       [text := "kill", enabled := False]
       focusOn input
       textCtrlSetEditable output False
       set f [layout := container p $
                        margin 10 $ column 5 [fill (widget output)
                                             ,row 5 [hfill (widget input), widget stop_]]
             ,clientSize  := sz 600 400
             ]

       let message txt = appendText output txt
       set input [on command := startProcess f input stop_ message]
       return ()
  where
    startProcess f input stop_ message
      = do txt <- get input text
           appendText input txt
           (send,_process,pid) <- processExecAsyncTimed f txt True {- process all input on termination -}
                                  (onEndProcess f input stop_ message) (onReceive message) (onReceive message)
           let sendLn txt_ = send (txt_ ++ "\n")
           if (pid /= 0)
            then do message ("-- start process: '" ++ txt ++ "' --\n")
                    set input [on command := sendCommand input sendLn]
                    set stop_  [enabled := True, on command  := unitIO (kill pid wxSIGKILL)]
            else return ()

    sendCommand input send
      = do txt_  <- get input text
           count <- comboBoxGetCount input
           appendText input txt_
           set input [selection := count]
           _ <- send txt_
           return ()


    onEndProcess f input stop_ message exitcode
      = do message ("\n-- process ended with exitcode " ++ show exitcode ++ " --\n")
           set input [on command := startProcess f input stop_ message]
           set stop_  [enabled := False, on command  := return ()]

    onReceive message txt_ _streamStatus
      = message txt_
