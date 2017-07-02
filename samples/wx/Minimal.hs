module Main where

import Graphics.UI.WX

main :: IO ()
main
  = start gui

gui :: IO ()
gui
  = do _ <- frame [text := "Hello world!"]
       return ()
