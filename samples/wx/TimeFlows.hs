{--------------------------------------------------------------------------------
  Copyright (c) 2003 Daan Leijen.

  "Time flows like a river" -- an old Fran demo :-)

  Demonstrates the use of an idle event handler to implement a resource
  aware gui that still does heavy animation.
--------------------------------------------------------------------------------}
module Main where

import System.CPUTime
import Graphics.UI.WXCore (getTextExtent)
import Graphics.UI.WX

{-------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------}
-- Time is in seconds, represented as a double.
type Time = Double

-- The (mouse) history consists of time/position pairs (and is never null)
type History = [(Time,Point)]


{-------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------}
-- The total time lag of the words behind the mouse cursor
timeSpan :: Time
timeSpan = 1

-- The flowing text
flowText :: String
flowText = "Time flows like a river"

-- The font style
flowFont :: FontStyle
flowFont = fontSwiss{ _fontSize = 16 }


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

       -- create a frame.
       f <- frame   [ text        := flowText]      
       p <- panel f []                          -- draw in a panel

       -- set event handlers
       set p        [ on paint    := onPaint  vmouseHistory 
                    , on idle     := onIdle   vmouseHistory p
                    , on drag     := onDrag   vmouseHistory
                    ]

       -- set layout
       set f        [ layout      := fill $ widget p
                    , clientSize  := sz 300 300       -- initial size
                    ]

{-------------------------------------------------------------------------
  Event handlers
-------------------------------------------------------------------------}
-- repaint handler
onPaint :: Var [(Time, Point)] -> DC a -> p -> IO ()
onPaint vmouseHistory  dc _viewArea
  = do history <- varGet vmouseHistory
       time    <- getTime
       -- draw trace line
       polyline dc (map snd history) [penColor := lightgrey]
       -- draw the words
       set dc [font := flowFont ]
       mapM_ drawWord (wordPositions history timeSpan time flowText)
  where
    drawWord (pos,word)
      = do -- center word
           sz_ <- getTextExtent dc word
           let newX = pointX pos - (sizeW sz_ `div` 2)
               newY = pointY pos - (sizeH sz_ `div` 2)
           -- and draw it.
           drawText dc word (pt  newX newY) []

           
-- idle event handler
onIdle :: Var History -> Window a -> IO Bool
onIdle vmouseHistory win
  = do history <- varGet vmouseHistory
       if (null (tail history))
        then do -- don't call idle again until some other event happens
                return False
        else do time <- getTime
                repaint win
                -- prune the history  
                varSet vmouseHistory (prune time history)
                return True
  where
    -- prune the history: only remember time/position pairs up to a certain time span.
    prune time (h:hs)
      = h:takeWhile (after (time-timeSpan)) hs
    prune _    []
      = undefined

    after time (t, _p)
      = time <= t


-- mouse drag handler
onDrag :: Var [(Time, b)] -> b -> IO ()
onDrag vmouseHistory mousePos_
  = do time <- getTime
       -- prepend a new time/position pair
       _ <- varUpdate vmouseHistory ((time,mousePos_):)
       return ()
           

{-------------------------------------------------------------------------
  Helper functions
-------------------------------------------------------------------------}
-- Tuple each word in a string with its historic position, given a mouse
-- history, a time span, and current time.
wordPositions :: History -> Time -> Time -> String -> [(Point,String)]
wordPositions history timeSpan_ time 
  = wordPositionsAt history . wordTimes timeSpan_ time . words 

-- Translate time/word pairs to position/word pairs given the mouse position history.
wordPositionsAt :: History -> [(Time,String)] -> [(Point,String)]
wordPositionsAt history timedWords
  = [(posAtTime t history, word) | (t,word) <- timedWords]

-- | Return the mouse position at a certain time.
posAtTime :: Time -> History -> Point
posAtTime _time [(_t,pos)]  = pos
posAtTime time ((t,pos):xs) | t <= time  = pos
                            | otherwise  = posAtTime time xs
posAtTime _    _            = undefined

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
