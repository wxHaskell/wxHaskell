{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DeriveDataTypeable, ScopedTypeVariables #-}
--------------------------------------------------------------------------------
{-|
Module      :  Classes
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

This modules defines attributes common to many widgets and
organizes them into Haskell classes. Look at the instance definitions
to see what kind of widgets support the attributes. 
    
Sometimes it is
hard to find what attributes a certain widget supports since the instance
definitions might be on some class higher in the hierarchy. For example,
many instances are defined for 'Window' @a@ -- this means that all
those attributes are applicable to any kind of 'Window', i.e. frames,
buttons, panels etc. However, these attributes will not be explicitly
listed at the type definitions of those classes.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Classes
    ( 
      -- * Data types
      Border(..)

      -- * Text
    , Textual(text,appendText)
    , Literate(font ,fontFamily, fontFace, fontSize, fontWeight, fontUnderline, fontShape
              ,textColor,textBgcolor)
      -- * Rendering
    , Dimensions(..)
    , Colored(..)
    , Visible(..)
    , Bordered(..)
      -- * Hierarchy
    , Child(..)
    , Parent(..)
    , Closeable(..)
      -- * Containers
    , Selection( selection )
    , Selections( selections )
    , Items( itemCount, item, items, itemAppend, itemDelete, itemsDelete )
      -- * Misc.
    , Able( enabled ) -- , enable
    , Help( help )
    , Tipped( tooltip )
    , Identity( identity )
    , Styled( style )
    , Framed( resizeable, maximizeable, minimizeable, closeable )
    , Checkable( checkable, checked )
    , Dockable( dockable )
    , Pictured( picture )
    , Valued( value )
    , Sized( size )
    , HasDefault( unsafeDefaultItem, defaultButton )
    ) where

import Data.Dynamic
import Graphics.UI.WXCore

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes


-- | Widgets with a label or text field.
class Textual w where
  -- | The text of a widget. It is interpreted differently for
  -- different widgets, for example, the title of a frame or the content of a
  -- static text control.
  text       :: Attr w String
  appendText :: w -> String -> IO ()

  appendText w s
    = set w [text :~ (++s)]


-- | Widgets with a picture.
class Pictured w where
  -- | The image of a widget.
  picture :: Attr w FilePath


-- | Widgets with a font.
class Literate w where
  -- | The font of the widget.
  font       :: Attr w FontStyle

  -- | The font size.
  fontSize   :: Attr w Int

  -- | The font weight.
  fontWeight :: Attr w FontWeight

  -- | The font family.
  fontFamily :: Attr w FontFamily

  -- | The font style.
  fontShape  :: Attr w FontShape

  -- | The font /face/: determines a platform dependent font.
  fontFace   :: Attr w String

  -- | Is the font underlined?
  fontUnderline :: Attr w Bool

  -- | Text color.
  textColor  :: Attr w Color

  -- | Text background color
  textBgcolor:: Attr w Color 

  fontSize      = mapAttr _fontSize   (\finfo x -> finfo{ _fontSize = x }) font
  fontWeight    = mapAttr _fontWeight (\finfo x -> finfo{ _fontWeight = x }) font
  fontFamily    = mapAttr _fontFamily (\finfo x -> finfo{ _fontFamily = x }) font
  fontShape     = mapAttr _fontShape  (\finfo x -> finfo{ _fontShape  = x }) font
  fontFace      = mapAttr _fontFace   (\finfo x -> finfo{ _fontFace = x }) font
  fontUnderline = mapAttr _fontUnderline (\finfo x -> finfo{ _fontUnderline = x }) font

  

-- | Widgets that have a size.
class Dimensions w where
  -- | The outer size of a widget (in pixels).
  outerSize :: Attr w Size
  -- | The (relative) position of a widget.
  position  :: Attr w Point
  -- | The occupied area.
  area      :: Attr w Rect
  -- | The preferred size of a widget.
  bestSize  :: ReadAttr w Size
  -- | The area available for client use (i.e. without the border etc).
  clientSize :: Attr w Size
  -- | The virtual size of a widget (ie. the total scrolling area)
  virtualSize :: Attr w Size

  -- defaults
  outerSize
    = mapAttr rectSize (\r sz' -> rect (rectTopLeft r) sz') area
  position
      = mapAttr rectTopLeft (\r pt' -> rect pt' (rectSize r)) area
  area
    = newAttr "area" getArea setArea
    where
      getArea w
        = do sz' <- get w outerSize
             pt' <- get w position
             return (rect pt' sz')
      setArea w rect'
        = set w [outerSize := rectSize rect', position := rectTopLeft rect']

  clientSize
    = outerSize
  bestSize
    = outerSize
  virtualSize
    = clientSize

class Colored w where
  -- | The background color.
  bgcolor    :: Attr w Color
  -- | The (foreground) color
  color      :: Attr w Color

  bgcolor
    = nullAttr "bgcolor"
  color
    = nullAttr "color"

-- | Visible widgets.
class Visible w where
  -- | Is the widget visible?
  visible    :: Attr w Bool
  -- | Refresh the widget explicitly.
  refresh    :: w -> IO ()

  -- | Should the widget be fully repainted on resize? This attribute only
  -- has effect when set at creation. If 'False', you will have to repaint
  -- the new window area manually at a resize, but flickering caused by
  -- background redraws can be prevented in this way. ('False' by default)
  fullRepaintOnResize :: Attr w Bool
  fullRepaintOnResize
    = nullAttr "fullRepaintOnResize"

  -- defaults
  visible
    = nullAttr "visible"
  refresh _w
    = return ()

-- | Parent widgets.
class Parent w where
  -- | Get the child widgets of a window.
  children :: ReadAttr w [Window ()]
  children = nullAttr "window"

  -- | Reduce flicker by not redrawing the background under child controls.
  -- This attribute has to be set at creation time. ('True' by default)
  clipChildren :: Attr w Bool
  clipChildren
    = nullAttr "clipChildren"


-- | Window borders
data Border = BorderSimple    -- ^ Displays a thin border around the window.  
            | BorderDouble    -- ^ Displays a double border. Windows only.  
            | BorderSunken    -- ^ Displays a sunken border.  
            | BorderRaised    -- ^ Displays a raised border.
            | BorderStatic    -- ^ Displays a border suitable for a static control. Windows only 
            | BorderNone      -- ^ No border
            deriving (Eq,Show,Read,Typeable)

instance BitMask Border where
  assocBitMask
    = [(BorderSimple, wxBORDER)
      ,(BorderDouble, wxBORDER_THEME)
      ,(BorderSunken, wxBORDER_SUNKEN)
      ,(BorderRaised, wxBORDER_RAISED)
      ,(BorderStatic, wxBORDER_STATIC)
      ,(BorderNone,   0)]


-- | Widgets with a border.
class Bordered w where
  -- | Specify the border of a widget.
  border :: Attr w Border
  border = nullAttr "border"


-- | Widgets that are part of a hierarchical order.
class Child w where
  -- | The parent widget.
  parent :: ReadAttr w (Window ())

  -- defaults
  parent
    = readAttr "parent" (\_w -> return objectNull)


-- | Widgets that can be closed.
class Closeable w where
  -- | Close the widget.
  close  :: w -> IO ()

-- | Widgets that have a system frame around them.
class Framed w where
  -- | Make the widget user resizeable? This attribute must be set at creation time.
  resizeable :: Attr w Bool
  resizeable  = nullAttr "resizeable"

  -- | Can the widget be minimized? This attribute must be set at creation time.
  minimizeable :: Attr w Bool
  minimizeable  = nullAttr "minimizeable"

  -- | Can the widget be maximized? This attribute must be set at creation time
  -- and is normally used together with 'resizeable'.
  maximizeable :: Attr w Bool
  maximizeable  = nullAttr "maximizeable"

  -- | Can the widget be closed by the user? This attribute must be set at creation time.
  closeable :: Attr w Bool
  closeable  = nullAttr "closeable"


-- | Widgets that can be enabled or disabled.
class Able w where
  -- | Enable, or disable, the widget.
  enabled :: Attr w Bool

-- | Widgets with help text.
class Help w where
  -- | Short help text, normally displayed in the status bar or popup balloon.
  help :: Attr w String

-- | Checkable widgets
class Checkable w where
  -- | Is the widget checkable?
  checkable :: Attr w Bool
  -- | Is the widget checked?
  checked   :: Attr w Bool

-- | The identity determines the wxWidgets ID of a widget.
class Identity w where
  -- | The identity determines the wxWidgets ID of a widget.
  identity :: Attr w Int


-- | The style is a bitmask that determines various properties of a widget.
class Styled w where
  -- | The windows style.
  style :: Attr w Int

-- | Dockable widgets.
class Dockable w where
  -- | Is the widget dockable?
  dockable :: Attr w Bool

-- | Widgets that have a tooltip
class Tipped w where
  -- | The tooltip information
  tooltip :: Attr w String

-- | Widgets with a single selection (radio group or listbox)
class Selection w where
  -- | The current selection as a zero-based index.
  -- Certain widgets return -1 when no item is selected.
  selection :: Attr w Int

-- | Widget with zero or more selections (multi select list boxes)
class Selections w where
  -- | The currently selected items in zero-based indices.
  selections :: Attr w [Int]


-- | Widgets containing certain items (like strings in a listbox)
class Items w a | w -> a where
  -- | Number of items.
  itemCount  :: ReadAttr w Int
  -- | All the items as a list. This attribute might not be writable for some widgets (like radioboxes)
  items  :: Attr w [a]
  -- | An item by zero-based index.
  item   :: Int -> Attr w a
  -- | Delete an item. Only valid for writeable items.
  itemDelete :: w -> Int -> IO ()
  -- | Delete all items. Only valid for writeable items.
  itemsDelete :: w -> IO ()
  -- | Append an item. Only valid for writeable items.
  itemAppend :: w -> a -> IO ()

  items
    = newAttr "items" getter setter
    where
      getter :: w -> IO [a]
      getter w
        = do n <- get w itemCount
             mapM (\i -> get w (item i)) [0..n-1]

      setter :: w -> [a] -> IO ()
      setter w xs
        = do itemsDelete w
             mapM_ (\x -> itemAppend w x) xs

  itemAppend w x
    = do xs <- get w items
         set w [items := xs ++ [x]]

  itemsDelete w
    = do count <- get w itemCount 
         sequence_ (replicate count (itemDelete w 0))

{--------------------------------------------------------------------------
  Values
--------------------------------------------------------------------------}  
-- | Items with a value.
class Valued w where
  -- | The value of an object.
  value :: Attr (w a) a

{--------------------------------------------------------------------------
  Size
--------------------------------------------------------------------------}  
-- | Sized objects (like bitmaps)
class Sized w where
  -- | The size of an object. (is 'outerSize' for 'Dimensions' widgets).
  size :: Attr w Size

{--------------------------------------------------------------------------
  HasDefault
--------------------------------------------------------------------------}  
-- | Objects which activate a 'Window' by default keypress
class HasDefault w where
  -- | Define a default item as any type deriving from 'Window'. Great care
  --   is required when using this option as you will have to cast the item
  --   to/from @Window ()@ using 'objectCast'. 
  --   For the common use case where the window in question is a 'Button', 
  --   please use 'defaultButton' as it is typesafe.
  unsafeDefaultItem :: Attr w (Window ())
  -- | Define the default button for a 'TopLevelWindow'. This is recommended
  --   for most use cases as the most common default control is a 'Button'.
  defaultButton :: Attr w (Button ())
