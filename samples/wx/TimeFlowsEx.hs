{--------------------------------------------------------------------------------
  Copyright (c) 2003 Daan Leijen.

  "Time flows like a river" -- an old Fran demo :-)

  Demonstrates the use of an idle event handler to implement a resource
  aware gui that still does heavy animation.

  This is an extended version of the "TimeFlows" demo that adds menus
  and dialogs to customize the appearance of the text, font, and delay.
--------------------------------------------------------------------------------}
module Main where

import System.CPUTime
import Graphics.UI.WXCore hiding (Time)-- (getTextExtent)
import Graphics.UI.WX

{-------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------}
-- Time is in seconds, represented as a double.
type Time = Double

-- The (mouse) history consists of time/position pairs (and is never null)
type History = [(Time,Point)]


{-------------------------------------------------------------------------
  The gui
-------------------------------------------------------------------------}
main :: IO ()
main
  = start timeFlows

timeFlows :: IO ()
timeFlows
  = do -- mouse history as list of time/position pairs: is never null!
       vmouseHistory <- varCreate [(0,pt 0 0)]
       -- the total time delay of the last word (in seconds)
       vtimeSpan     <- varCreate 1.0
       -- the text
       vflowText     <- varCreate "time flows like a river"
       -- the font
       vflowFont     <- varCreate (fontSwiss{ _fontSize = 16 })
       -- show mouse track?
       vflowLine     <- varCreate True

       -- create a frame and panel to draw in.
       f <- frame   [ text        := "Time flows"]
       p <- panel f []

       -- create menus & status fields       
       mfile    <- menuPane       [text := "&File"]
       _mquit   <- menuQuit mfile [help := "Exit the demo", on command := close f]

       medit    <- menuPane       [text := "&Edit"]
       mshowline<- menuItem medit [text := "&Show mouse track\tCtrl+S", checkable := True, checked := True]
       mfont    <- menuItem medit [text := "&Font...\tCtrl+F", help := "Set the font"]
       moptions <- menuItem medit [text := "&Options...\tCtrl+O", help := "Edit demo options"]
       
       mhelp    <- menuHelp        []
       mabout   <- menuAbout mhelp [help := "Information about this demo."]

       flowText <- varGet vflowText
       status   <- statusField [text := flowText]

       -- set layout (before the menubar!)
       set f [ layout      := fill $ widget p
             , clientSize  := sz 300 300        --initial size
             ]

       -- set menu and status bar
       set f [ menuBar     := [mfile,medit,mhelp]
             , statusBar   := [status]
             , on (menu mabout)    := infoDialog f "About Time flows.." "This is an idle event application."
             , on (menu mshowline) := do showit <- get mshowline checked                                 
                                         varSet vflowLine showit
             , on (menu moptions)  := showOptionDialog f vtimeSpan vflowText status
             , on (menu mfont)     := do _ <- showFontDialog f vflowFont; return ()
             , on (charKey '+')    := do _ <- varUpdate vtimeSpan (\n -> min 10 (n+1)); return ()
             , on (charKey '-')    := do _ <- varUpdate vtimeSpan (\n -> max  1 (n-1)); return ()
             ]
      
       -- set event handlers
       set p [ on paint    := onPaint  vmouseHistory vtimeSpan vflowLine vflowText vflowFont
             , on idle     := onIdle   vmouseHistory vtimeSpan p
             , on drag     := onDrag   vmouseHistory
             ]

       return ()

{-------------------------------------------------------------------------
  Dialogs
-------------------------------------------------------------------------}
showFontDialog :: Window a -> Var FontStyle -> IO ()
showFontDialog frame_ vflowFont
  = do flowFont <- varGet vflowFont
       mbfont   <- fontDialog frame_ flowFont
       case mbfont of
         Nothing    -> return ()
         Just font_ -> varSet vflowFont font_

showOptionDialog :: (Textual w, RealFrac a) =>
                    Window b -> Var a -> Var String -> w -> IO ()
showOptionDialog frame_ vtimeSpan vflowText status
  = do flowText <- varGet vflowText
       timeSpan <- varGet vtimeSpan
      
       -- create dialog
       d     <- dialog frame_ [text := "Options", resizeable := True]
       p     <- panel d []
       ntry  <- textEntry  p [text := flowText]
       delay <- spinCtrl  p 1 10 [selection := round timeSpan]
       ok    <- button p [text := "Ok"] 
       can   <- button p [text := "Cancel"]           
       
       -- layout
       set d [defaultButton := ok
             ,layout := container p $ margin 10 $ 
                        column 10 [ boxed "" $ grid 5 5 [[label "text:",  hfill $ widget ntry]
                                                        ,[label "delay:", floatRight $ widget delay]
                                                        ]
                                  , floatBottomRight $ row 5 [widget ok, widget can]
                                  ]
             ]

       -- show modal 
       ret   <- showModal d $ \stop_ -> 
                do set ok  [on command := do flowText_ <- get ntry  text 
                                             timeSpan_ <- get delay selection
                                             stop_ (Just (flowText_, fromIntegral timeSpan_))]
                   set can [on command := stop_ Nothing]
                   
       -- set results
       case ret of
         Nothing -> return ()
         Just (flowText_, timeSpan_)
                 -> do varSet vflowText flowText_
                       set status [text := flowText_]
                       varSet vtimeSpan timeSpan_


{-------------------------------------------------------------------------
  Event handlers
-------------------------------------------------------------------------}
-- repaint handler
onPaint :: Var [(Time, Point)]
           -> Var Time
           -> Var Bool
           -> Var String
           -> Var FontStyle
           -> DC a
           -> p
           -> IO ()
onPaint vmouseHistory vtimeSpan vflowLine vflowText vflowFont  dc _viewArea
  = do time      <- getTime
       history   <- varGet vmouseHistory
       timeSpan_ <- varGet vtimeSpan
       flowFont  <- varGet vflowFont
       flowText  <- varGet vflowText
       flowLine  <- varGet vflowLine

       -- draw trace line
       when (flowLine) (polyline dc (map snd history) [penColor := lightgrey])
       -- draw the words
       set dc [font := flowFont ]
       mapM_ drawWord (wordPositions history timeSpan_ time flowText)
  where
    drawWord (pos,word)
      = do -- center word
           sz_ <- getTextExtent dc word
           let newX = pointX pos - (sizeW sz_ `div` 2)
               newY = pointY pos - (sizeH sz_ `div` 2)
           -- and draw it.
           drawText dc word (pt newX newY) []

           
-- idle event handler
onIdle :: Var History -> Var Time -> Window a -> IO Bool
onIdle vmouseHistory vtimeSpan win_
  = do history <- varGet vmouseHistory
       if (null (tail history))
        then do -- don't call idle again until some other event happens
                return False
        else do time     <- getTime
                timeSpan <- varGet vtimeSpan
                repaint win_
                -- prune the history  
                varSet vmouseHistory (prune (time - timeSpan) history)
                return True
  where
    -- prune the history: only remember time/position pairs up to a certain time span.
    prune time (h:hs)
      = h:takeWhile (after time) hs
    prune _    []
      = undefined

    after time (t, _p)
      = time <= t


-- mouse drag handler
onDrag :: Var [(Time, b)] -> b -> IO ()
onDrag vmouseHistory mousePos_
  = do time <- getTime
       -- prepend a new time/position pair
       _    <- varUpdate vmouseHistory ((time,mousePos_):)
       return ()


{-------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------}
-- Tuple each word in a string with its historic position, given a mouse
-- history, a time span, and current time.
wordPositions :: History -> Time -> Time -> String -> [(Point,String)]
wordPositions history timeSpan time 
  = wordPositionsAt history . wordTimes timeSpan time . words 

-- Translate time/word pairs to position/word pairs given the mouse position history.
wordPositionsAt :: History -> [(Time,String)] -> [(Point,String)]
wordPositionsAt history timedWords
  = [(posAtTime t history, word) | (t,word) <- timedWords]

-- | Return the mouse position at a certain time.
posAtTime :: Time -> History -> Point
posAtTime _time [(_t,pos)]   = pos
posAtTime time  ((t,pos):xs) | t <= time  = pos
                             | otherwise  = posAtTime time xs
posAtTime _     []           = undefined

-- | Evenly assign times to the words in a string, given a timeSpan and current time.
wordTimes :: Time -> Time -> [String] -> [(Time,String)]
wordTimes timeSpan_ time words_
  = let n     = length words_
        delta = timeSpan_ / (fromIntegral n)
    in zip (iterate (\t -> t-delta) time) words_
    
-- Get the current Time
getTime :: IO Time
getTime
  = do picoSecs <- getCPUTime
       let time = (fromIntegral picoSecs) / 1.0e12
       return time
