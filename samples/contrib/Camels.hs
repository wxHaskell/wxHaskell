{--------------------------------------------------------------------------------
   Camel game by Maarten Löfler (mloffler@cs.uu.nl) (adapted by Daan Leijen).
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore

import Paths_samplesContrib

{--------------------------------------------------------------------------------
   The game
--------------------------------------------------------------------------------}
type Board = [Field]
data Field = Full Camel | Empty deriving Eq
data Camel = East | West        deriving Eq

newBoard :: Int -> Board
newBoard x | even x    = error "board size must be odd"
           | x < 3     = error "board size must be at least 3"
           | otherwise = let c = x `div` 2
                         in replicate c (Full East) ++ Empty : replicate c (Full West)

correct :: Board -> Bool
correct fs = let c = length fs `div` 2
             in all (== Full West) (take c       fs)
             && all (== Full East) (drop (c + 1) fs)

(|>) :: Int -> Field -> Board -> Board
(|>) _ _ [] = []
(|>) 0 f (_:bs) = f : bs
(|>) x f (b:bs) = b : (|>) (x - 1) f bs

moveAllowed :: Int -> Board -> Bool
moveAllowed x b | x <  0          = False
                | x >= length b   = False
                | b !! x == Empty = False
                | otherwise = case b !! x of Full East | x >= length b - 1         -> False
                                                       | b !! (x + 1) == Empty     -> True
                                                       | b !! (x + 1) == Full East -> False
                                                       | x >= length b - 2         -> False
                                                       | b !! (x + 2) == Empty     -> True
                                                       | otherwise                 -> False
                                             Full West | x < 1                     -> False
                                                       | b !! (x - 1) == Empty     -> True
                                                       | b !! (x - 1) == Full West -> False
                                                       | x < 2                     -> False
                                                       | b !! (x - 2) == Empty     -> True
                                                       | otherwise                 -> False
                                             Empty                                 -> False

move :: Int -> Board -> Board
move x b = case b !! x of Full East -> case b !! (x + 1) of Empty -> (x |> Empty) . ((x + 1) |> Full East) $ b
                                                            _     -> (x |> Empty) . ((x + 2) |> Full East) $ b
                          Full West -> case b !! (x - 1) of Empty -> (x |> Empty) . ((x - 1) |> Full West) $ b
                                                            _     -> (x |> Empty) . ((x - 2) |> Full West) $ b
                          _         -> b

{--------------------------------------------------------------------------------
   The GUI
--------------------------------------------------------------------------------}
main :: IO ()
main = start gui

gui :: IO ()
gui
  = do desert <- varCreate (newBoard 3)
       imagePath <- getDataFileName "bitmaps/desert.bmp"
       b <- bitmapCreateLoad imagePath wxBITMAP_TYPE_BMP
       f <- frame    [ text := "Camels", on closing := do bitmapDelete b; propagateEvent ]
       q <- button f [ text := "quit" , on command := close f ]
       h <- button f [ text := "help" , on command := chelp f ]
       a <- button f [ text := "about", on command := about f ]
       p <- panel  f [ clientSize := sz 320 240 ]

       set p [ on resize := repaint p
             , on click  := klik p desert
             , on paint  := drawDesert desert b 
             ]
       set f [ layout := column 0
                         [ fill $ widget p
                         , hfloatCentre $ margin 5 $ row 5 [widget q, widget h, widget a]
                         ]
             , defaultButton := q
             ]
       return ()

drawDesert :: Var Board -> Bitmap () -> DC () -> Rect -> IO ()
drawDesert desert bmp dc (Rect x y w h) =
  do drawBitmap dc bmp pointZero False []
     for 0 (w `div` 234) (\i ->
       for 0 (h `div` 87) (\j ->
         drawBitmap dc bmp (pt (i * 234) (j * 87)) False []))
     board <- varGet desert
     let l = length board
         s = min h $ w `div` l
         xd = x + (w - l * s) `div` 2
         yd = y + (h -     s) `div` 2
     for 0 (l - 1) (\i -> drawField dc (board !! i) (Rect (xd + i * s) yd s s))
     return ()

for :: Int -> Int -> (Int -> IO ()) -> IO ()
for x y f = sequence_ $ map f [x..y]

drawField :: DC () -> Field -> Rect -> IO ()
drawField dc f r@(Rect _x _y _w _h) =
  do set dc [brushKind := BrushSolid, brushColor := rgb 0x80 0x80 (0x00 :: Int)] 
     case f of Full East -> do polygon dc (map (lin r) camel) []
                               polygon dc (map (lin r) saddle) [brushColor := red] 
               Full West -> do polygon dc (map (lin r . mirror) camel) []
                               polygon dc (map (lin r . mirror) saddle) [brushColor := blue] 
               _         -> return ()

camel :: [(Float, Float)]
camel = map (\(x, y) -> (x / 8, y / 8)) [(2, 2), (3, 3), (4, 2), (5, 3), (6, 2), (7, 3), (6, 3), (5, 5), (5, 7), (4, 7), (4, 5), (3, 5), (3, 7), (2, 7), (2, 4), (1, 6), (1, 4)]

saddle :: [(Float, Float)]
saddle = map (\(x, y) -> (x / 8, y / 8)) [(4, 2), (5, 3), (4, 4), (3, 3)]

mirror :: (Float, Float) -> (Float, Float)
mirror (x, y) = (1 - x, y)

lin :: Rect -> (Float, Float) -> Point
lin (Rect x y w h) (px, py) = let nx = floor $ (fromInteger . toInteger) w * px
                                  ny = floor $ (fromInteger . toInteger) h * py
                              in Point (x + nx) (y + ny)

klik :: Panel () -> Var Board -> Point -> IO ()
klik pan desert (Point x _y) =
  do board <- varGet desert
     (Size w h) <- get pan clientSize
     let l = length board
         s = min h $ w `div` l
         xd = (w - l * s) `div` 2
         _yd = (h -     s) `div` 2
         i = (x - xd) `div` s
     _ <- varUpdate desert (if moveAllowed i board then move i else id)
     newboard <- varGet desert
     repaint pan
     eind pan desert newboard
     return ()

eind :: Panel () -> Var Board -> Board -> IO ()
eind pan desert board
  | any (flip moveAllowed board) [0 .. length board - 1] = return ()
  | correct board = do infoDialog pan "Level up" "Congratulations! You succeeded."
                       _ <- varUpdate desert (const $ newBoard $ length board + 2)
                       repaint pan
  | otherwise     = do infoDialog pan "Level restart" "There are no more possible moves..."
                       _ <- varUpdate desert (const $ newBoard $ max 3 $ length board)
                       repaint pan

about :: Window a -> IO ()
about w
  = infoDialog w "About Camels" "Camels\n\nby Maarten Löfler\nmloffler@cs.uu.nl\n\nCamels was written using wxHaskell"

chelp :: Window a -> IO ()
chelp w
  = infoDialog w "Camels Help"
  (  "How to play Camels\n\n"
  ++ "The object of this puzzle is to move all the east looking camels to the eastern\n"
  ++ "end of the desert, and all the west looking camels to the west of the desert.\n"
  ++ "East looking camels can only move east, and west looking camels can only move\n"
  ++ "west. A camel can move one square forward (if that square is empty), or it can\n"
  ++ "jump over another camel if it is looking the OTHER way.\n\n"
  ++ "Once you succeed, you will advance to a higher level with more camels.\n\n"
  ++ "Good luck!\n"
  )
