{--------------------------------------------------------------------------------
   Test Grid.
--------------------------------------------------------------------------------}
module Main where
 
import Graphics.UI.WX
import Graphics.UI.WXCore hiding (Event)

main :: IO ()
main  
  = start gui

gui :: IO ()
gui 
  = do f <- frame [text := "Grid test", visible := False] 
           
       -- use text control as logger
       textlog <- textCtrl f [wrap := WrapNone, enabled := False] 
       textCtrlMakeLogActiveTarget textlog
       logMessage "logging enabled"              

       -- grids
       g <- gridCtrl f []
       gridSetGridLineColour g (colorSystem Color3DFace)
       gridSetCellHighlightColour g black
       appendColumns g (head names)
       appendRows    g (map show [1..length (tail names)])
       mapM_ (setRow g) (zip [0..] (tail names))
       gridAutoSize g

       windowOnKeyDown g (onGridKeyDown g)
       set g [on gridEvent := onGrid]

       -- layout
       set f [layout := column 5 [fill (dynamic (widget g))
                                 ,hfill $ minsize (sz 20 80) $ widget textlog]
             ]       
       focusOn g
       set f [visible := True]  -- reduce flicker at startup.
       return ()
  where
    onGridKeyDown g (EventKey key_ _mods _pt)
      = case key_ of
          KeyReturn ->          
            do logMessage "keyEnter"
               gridMoveNext g
          _ -> propagateEvent

    onGrid ev
      = case ev of
          GridCellChange row_ col_ _veto
            -> logMessage ("cell changed: " ++ show (row_, col_))
          _ -> propagateEvent

names :: [[String]]
names
  = [["First Name", "Last Name"]
    ,["Daan","Leijen"],["Arjan","van IJzendoorn"]
    ,["Martijn","Schrage"],["Andres","Loh"]]


setRow :: Grid a -> (Int, [String]) -> IO ()
setRow g (row_, values)
  = mapM_ (\(col,value_) -> gridSetCellValue g row_ col value_) (zip [0..] values)


{--------------------------------------------------------------------------------
   Library?
--------------------------------------------------------------------------------}

gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl parent_ props_
  = feed2 props_ 0 $
    initialWindow $ \id_ rect' -> \props' flags ->
    do g <- gridCreate parent_ id_ rect' flags
       gridCreateGrid g 0 0 0
       set g props'
       return g

gridEvent :: Event (Grid a) (EventGrid -> IO ())
gridEvent
  = newEvent "gridEvent" gridGetOnGridEvent gridOnGridEvent


gridMoveNext :: Grid a -> IO ()
gridMoveNext g
  = do row_ <- gridGetGridCursorRow g
       col_ <- gridGetGridCursorCol g
       rowCount <- gridGetNumberRows g
       colCount <- gridGetNumberCols g
       let (r,c) = if (row_ + 1 >= rowCount)
                    then if (col_ + 1 >= colCount)
                     then (0, 0)
                     else (0, col_ + 1)
                    else (row_ + 1, col_)
       gridSetGridCursor g r c
       gridMakeCellVisible g r c


appendColumns :: Grid a -> [String] -> IO ()
appendColumns _g []
  = return ()
appendColumns g labels
  = do n <- gridGetNumberCols g
       _ <- gridAppendCols g (length labels) True
       mapM_ (\(i, label_) -> gridSetColLabelValue g i label_) (zip [n..] labels)

appendRows :: Grid a -> [String] -> IO ()
appendRows _g []
  = return ()
appendRows g labels
  = do n <- gridGetNumberRows g
       _ <- gridAppendRows g (length labels) True
       mapM_ (\(i, label_) -> gridSetRowLabelValue g i label_) (zip [n..] labels)

