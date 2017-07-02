-- an eyeball test for unicode handling
-- loads a file of UTF-8 encoded strings (hardcoded below) and displays widgets
-- for each one of the strings
-- Eric Kow 2006

{-# LANGUAGE FlexibleContexts      #-}

module Main where

import UTF8
import Paths_samplesTest

import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO
import Control.Monad (liftM)
import Data.Word (Word8)
import Foreign.Marshal.Array

main :: IO ()
main = start sampler

sampler :: IO ()
sampler =
 do f  <- frame [text := "UTF-8 viewer"]
    pnl <- panel f []
    nb  <- notebook pnl []
    --
    sLines <- liftM lines readTestFile
    let bunchOf lable thing =
         do p  <- panel nb []
            ws <- mapM (\t -> thing p [ text := t, tooltip := t ]) sLines
            return $ tab lable $ container p $ margin 10 $ column 1 $ map widget ws
        bunchOfSel lable thing =
         do p <- panel nb []
            w <- thing p [on select ::= logSelect sLines]
            return $ tab lable $ container p $ margin 10 $ widget w
    -- manually created tabs
    p1 <- panel nb []
    let for1 thing =
          do w <- thing p1 [ on select ::= logSelect sLines, items := sLines ]
             set w [ selection := 0 ]
             return w
    p1choice <- for1 choice
    p1combo  <- for1 comboBox
    p1slist  <- for1 singleListBox
    p1mlist  <- multiListBox p1 [ items := sLines ]
    let t1 = tab "Selectors" $ container p1 $ margin 10 $ column 1 $
             [ label "choice", widget p1choice
             , label "combo" , widget p1combo
             , label "s-list", widget p1slist
             , label "m-list", widget p1mlist ]
    p2 <- panel nb []
    let t2 = tab "Labels" $ container p2 $ column 1 $ map label sLines
    --
    textlog <- textCtrl pnl [enabled := False, wrap := WrapNone]
    textCtrlMakeLogActiveTarget textlog
    logMessage "logging enabled"
    --
    ts <- sequence [ bunchOf "Static" staticText
                   , bunchOf "TextEntry" textEntry
                   , bunchOf "Checks" checkBox
                   , bunchOf "Buttons" button
                   , bunchOfSel "Radio"  (\p -> radioBox p Vertical sLines)
                   ]
    --
    set f [layout := container pnl $ hfill $ column 0
            [ hfill $ tabs nb $ t1:t2:ts
            , hfill $ widget textlog ]
          ]
 where
    logSelect _labels w
      = do i <- get w selection
           s <- get w (item i)
           logMessage ("selected index: " ++ show i ++ ": " ++ s)

--  from the mailing list
hGetBytes :: Handle -> Int -> IO [Word8]
hGetBytes h c = allocaArray c $ \p ->
                  do c' <- hGetBuf h p c
                     peekArray c' p

readTestFile :: IO String
readTestFile =
 do
    testFilePath <- getDataFileName testFile
    h  <- openBinaryFile testFilePath ReadMode
    hsize <- hFileSize h
    ws <- hGetBytes h $ fromIntegral hsize
    return . fst . decode $ ws

testFile :: FilePath
testFile = "utf-tests"
