{-# LANGUAGE FlexibleContexts      #-}

module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore

main :: IO ()
main 
  = start $ 
    do p <- getApplicationDir
       f <- frame [text := p]
       h <- staticText f [text := "Hello world!", on click ::= goodbye]
       b <- button     f [text := "Ok", on command ::= longlabel]
       set f [layout := column 5 [widget h, widget b]]
       set f [clientSize := sz 300 300]

longlabel :: Button () -> IO ()
longlabel b
  = do set b [text        := "a really long label for a button"
             ,on command ::= shortlabel]
       refit b

shortlabel :: Button () -> IO ()
shortlabel b
  = do set b [text        := "short"
             ,on command ::= longlabel]
       refit b

goodbye :: StaticText () -> Point -> IO ()
goodbye st _pos
  = do set st [text      := "Goodbye world!\nAlas, I knew it well..."
              ,on click ::= hello]
       refit st

hello :: StaticText () -> Point -> IO ()
hello st _pos
  = do set st [text      := "Hi!"
              ,on click ::= goodbye]
       refitMinimal st
