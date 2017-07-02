{--------------------------------------------------------------------------------
 Copyright (c) Daan Leijen 2004
 wxWindows License.

 Demonstration of making a custom color box control in wxHaskell
 that behaves just like any other widget.
--------------------------------------------------------------------------------}
module Main where

import Graphics.UI.WX

-- main gui.
main :: IO ()
main 
  = start $ 
    do f    <- frame [text := "Custom color box controls"]
       c    <- colorBox f [boxcolor := red]

       set f [layout := floatCenter $ widget c
             ,clientSize := sz 280 100]


-- Define a new custom "ColorBox" as a subclass of "Window".
type ColorBox a  = Window (CColorBox a)
data CColorBox a = CColorBox

-- Create our new ColorBox control.
-- Unfortunately, casting is involved here as we want to allow
-- all normal window attributes; this means that one has to define
-- the type signature of "colorBox" carefully.
colorBox :: Window a -> [Prop (ColorBox ())] -> IO (ColorBox ())
colorBox parent_ props
  = do let defaults  = [clientSize := sz 20 20, border := BorderStatic]
           cboxprops = castProps cast props -- cast properties to colorbox properties
       w <- window parent_ (defaults ++ cboxprops)
       let cbox = cast w                    -- cast to our new color box type
       set cbox [on click := selectColor cbox]
       return cbox
  where
    -- select a new color for the colorbox control.
    selectColor cbox _pt
      = do c   <- get cbox boxcolor
           mbc <- colorDialog cbox c
           case mbc of
             Just c_  -> set cbox [boxcolor := c_]
             Nothing -> return ()

    -- our casting operator: type signature is necessary!
    cast :: Window a -> ColorBox ()
    cast = objectCast

-- Define a custom attribute
-- This is easy as we map to the background color. For more complicated
-- things, one should define a ColorBox data type instead of a simple
-- type synonym.
boxcolor :: Attr (ColorBox a) Color
boxcolor 
  = newAttr "boxcolor" getter setter
  where
    getter cbox       = get cbox bgcolor
    setter cbox clr   = do set cbox [bgcolor := clr]
                           refresh cbox

