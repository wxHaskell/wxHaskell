{--------------------------------------------------------------------------------
This program implements the "goodbye" demo as posted by John Meacham on
the Haskell GUI mailing list. The program is specified as:

I propose a simple program which pops up a window saying 'Hello World'
with a button saying 'Bye' which you click and it changes the message
to 'Goodbye'. If you click the button again the program exits.

When the button is clicked the first time, it calls "onCommand1". This function
changes the text of the label and installs another event handler on the button
that closes the main frame. (by default, wxWidgets exits the gui when all
windows are closed).
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WXCore

main :: IO ()
main
  = run gui

gui :: IO ()
gui
  = do -- create top frame
       f <- frameCreateTopFrame "Bye demo"
       _ <- windowSetBackgroundColour f white

       -- panel for automatic tab management and the nice background
       p <- panelCreate f idAny rectNull 0

       -- create text and button control
       t <- staticTextCreate p idAny "Hello World" rectNull 0
       b <- buttonCreate p idAny "Bye" rectNull 0

       -- press spacebar to invoke the button by default
       topLevelWindowSetDefaultItem f b

       -- set the layout
       windowSetLayout f (fill (container p (margin 5 (column 5 [widget t, widget b]))))

       -- set command handler on the button
       buttonOnCommand b (onCommand1 f t b)

       -- show the frame
       _ <- windowShow f
       windowRaise f
       return ()
  where
    -- call on the first click
    onCommand1 f t b
      = do controlSetLabel t "Goodbye!"
           buttonOnCommand b (onCommand2 f)
           return ()

    -- call on the second click
    onCommand2 f
      = do _ <- windowClose f False {- can veto -}
           return ()
