{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
--------------------------------------------------------------------------------
{-|	Module      :  Controls
	Copyright   :  (c) Daan Leijen 2003
	               (c) Shelarcy (shelarcy@gmail.com) 2006
	License     :  wxWindows

	Maintainer  :  wxhaskell-devel@lists.sourceforge.net
	Stability   :  provisional
	Portability :  portable

Defines common GUI controls.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WX.Controls
    ( -- * Classes
        Align(..), Aligned, alignment
      , Wrap(..), Wrapped, wrap
      , Sorted, sorted
      -- * Containers
      , Panel, panel, panelEx
      , Notebook, notebook
      , focusOn
      -- * Controls
      -- ** Button
      , Button, button, buttonEx, smallButton, buttonRes
      , BitmapButton, bitmapButton, bitmapButtonRes
      -- ** Text entry
      , TextCtrl, entry, textEntry, textCtrl, textCtrlRich, textCtrlEx
      , textCtrlRes, processEnter, processTab
      -- ** CheckBox
      , CheckBox, checkBox, checkBoxRes
      -- ** Choice
      , Choice, choice, choiceEx, choiceRes
      -- ** ComboBox
      , ComboBox, comboBox, comboBoxEx, comboBoxRes
      -- ** ListBox
      , ListBox, SingleListBox, MultiListBox
      , singleListBox, singleListBoxRes, multiListBox, multiListBoxRes
      -- ** RadioBox
      , RadioBox, radioBox, radioBoxRes
      -- ** Spin Control
      , SpinCtrl, spinCtrl, spinCtrlRes
      -- ** Slider
      , Slider, hslider, vslider, sliderEx, sliderRes
      -- ** Gauge
      , Gauge, hgauge, vgauge, gaugeEx, gaugeRes
      -- ** ToggleButton
      , ToggleButton, BitmapToggleButton
      , toggleButton, bitmapToggleButton
      -- ** Tree control
      , TreeCtrl, treeCtrl, treeCtrlEx, treeEvent, treeCtrlRes
      -- ** List control
      , ListCtrl, listCtrl, listCtrlEx, listCtrlRes, listCtrlSetColumnWidths, listEvent, columns
      , ListView(..), listViewLayout, listViewSetHandler, listViewSelectHandle, listViewSetItems, listViewGetItems, listViewAddItem, listView
      -- ** Static text
      , StaticText, staticText, staticTextRes
      -- ** SplitterWindow
      , SplitterWindow, splitterWindow
      -- ** ImageList
      , ImageList, imageList, imageListFromFiles
      -- ** MediaCtrl
      , MediaCtrlBackend(..), MediaCtrl, mediaCtrl, mediaCtrlWithBackend, mediaCtrlEx
      -- ** StyledTextCtrl
      , StyledTextCtrl, stcEvent, styledTextCtrl, styledTextCtrlEx
      -- ** PropertyGrid
      , PropertyGrid, propertyGrid, propertyGridEvent
    ) where

import Graphics.UI.WXCore hiding (Event)

import Graphics.UI.WX.Types
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events
import Graphics.UI.WX.Layout
import Graphics.UI.WX.Media (Media(..))
import Graphics.UI.WX.Variable (variable)
import Graphics.UI.WX.Window

import Control.Monad (forM_)
import Data.Dynamic  -- for "alignment"
import System.Info (os)


defaultStyle
  = wxCLIP_CHILDREN -- .+. wxNO_FULL_REPAINT_ON_RESIZE

-- | Create a 'Panel', a window that is normally used as a container for
-- controls. It has a standard background and maintains standard keyboard
-- navigation (ie. /Tab/ moves through the controls).
--
-- Note: 'defaultButton' attibute is removed. Set 'defaultButton' to parent
-- 'Frame' or 'Dialog' instead of this control now. This is an incompatible
-- change to support wxWidgets 2.8.x.
--
-- * Attributes: 'focusOn'
--
-- * Instances: 'Form' -- 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled',
--             'Textual', 'Literate'
panel :: Window a -> [Prop (Panel ())] -> IO (Panel ())
panel parent props
  = panelEx parent (wxTAB_TRAVERSAL .+. defaultStyle) props


-- | Create a 'Panel' with a specific style.
--
-- Note: 'defaultButton' attibute is removed. Set 'defaultButton' to parent
-- 'Frame' or 'Dialog' instead of this control now. This is an incompatible
-- change to support wxWidgets 2.8.x.
--
-- * Attributes: 'focusOn'
--
-- * Instances: 'Form' -- 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled',
--             'Textual', 'Literate', 'Reactive', 'Paint'
panelEx :: Window a -> Style -> [Prop (Panel ())] -> IO (Panel ())
panelEx parent style props
  = feed2 props style $
    initialContainer $ \id rect -> \props flags  ->
    do p <- panelCreate parent id rect flags
       windowSetFocus p
       set p props
       return p

instance Form (Panel a) where
  layout
    = writeAttr "layout" windowSetLayout

-- | Set the initial focus on this control.
focusOn :: Window a -> IO ()
focusOn w
  = windowSetFocus w


-- | Create a 'Notebook'. Layout is managed with the 'tabs' combinator.
--
-- * Instances: 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled',
--             'Textual', 'Literate', 'Reactive', 'Paint'
notebook :: Window a -> [Prop (Notebook ())] -> IO (Notebook ())
notebook parent props
  = feed2 props defaultStyle $
    initialContainer $ \id rect -> \props flags ->
    do nb <- notebookCreate parent id rect flags
       set nb props
       return nb

{--------------------------------------------------------------------------------
  Button
--------------------------------------------------------------------------------}

-- | Create a standard push button.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
button :: Window a -> [Prop (Button ())] -> IO (Button ())
button parent props
  = buttonEx parent 0 props

-- | Create a minimially sized push button.
--
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
smallButton :: Window a -> [Prop (Button ())] -> IO (Button ())
smallButton parent props
  = buttonEx parent wxBU_EXACTFIT props


-- | Create a standard push button with the given flags.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
buttonEx :: Window a -> Style -> [Prop (Button ())] -> IO (Button ())
buttonEx parent stl props
  = feed2 props stl $
    initialWindow $ \id rect ->
    initialText   $ \txt -> \props flags ->
    do b <- buttonCreate parent id txt rect flags
       set b props
       return b

-- | Complete the construction of a push button instance which has been loaded
--   from a resource file.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
buttonRes :: Window a -> String -> [Prop (Button ())] -> IO (Button ())
buttonRes parent name props =
    do b <- xmlResourceGetButton parent name
       set b props
       return b

instance Commanding (Button a) where
  command  = newEvent "command" buttonGetOnCommand buttonOnCommand

-- | Create a bitmap button. Use the 'image' attribute to set the
-- bitmap.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
bitmapButton :: Window a -> [Prop (BitmapButton ())] -> IO (BitmapButton ())
bitmapButton parent props
  = feed2 props wxBU_AUTODRAW $
    initialWindow $ \id rect -> \props flags ->
    do bb <- bitmapButtonCreate parent id nullBitmap rect flags
       set bb props
       return bb

-- | Complete the construction of a bitmap button instance which has been loaded
--   from a resource file.
--
-- * Instances: 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
bitmapButtonRes :: Window a -> String -> [Prop (BitmapButton ())] -> IO (BitmapButton ())
bitmapButtonRes parent name props =
    do b <- xmlResourceGetBitmapButton parent name
       set b props
       return b

instance Pictured (BitmapButton a) where
  picture
    = writeAttr "picture" setter
    where
      setter w fname
        = do fpath <- getAbsoluteFilePath fname
             withBitmapFromFile fpath (bitmapButtonSetBitmapLabel w)


{--------------------------------------------------------------------------------
  Text entry
--------------------------------------------------------------------------------}
-- | Alignment.
data Align   = AlignLeft | AlignRight | AlignCentre
             deriving (Eq,Show,Read,Typeable)

-- | Wrap mode.
data Wrap    = WrapNone   -- ^ No wrapping (and show a horizontal scrollbar).
             | WrapLine   -- ^ Wrap lines that are too long at any position.
             | WrapWord   -- ^ Wrap lines that are too long at word boundaries.
             deriving (Eq,Show,Read,Typeable)

instance BitMask Align where
  assocBitMask
    = [(AlignCentre,wxALIGN_CENTRE_HORIZONTAL)
      ,(AlignRight, wxALIGN_RIGHT)
      ,(AlignLeft,  wxALIGN_LEFT)]

instance BitMask Wrap where
  assocBitMask
    = [(WrapNone, wxHSCROLL)
      ,(WrapLine, wxTE_LINEWRAP)
      ,(WrapWord, wxTE_WORDWRAP)]


-- | Widgets that can have aligned content.
-- Note: this property is not
-- used to set the alignment of a widget itself -- See "Graphics.UI.WXCore.Layout"
-- for more information about layout.
class Aligned w where
  -- | Set the alignment of the content. Due to wxWidgets constrictions,
  -- this property has to be set at creation time.
  alignment :: CreateAttr w Align

initialAlignment :: Aligned w => ([Prop w] -> Style -> a) -> [Prop w] -> Style -> a
initialAlignment cont props style
  = case filterProperty alignment props of
      (PropValue x, ps)  -> cont ps (setBitMask x style)
      (PropModify f, ps) -> cont ps (setBitMask (f (fromBitMask style)) style)
      (PropNone, ps)     -> cont ps style


instance Aligned (TextCtrl a) where
  alignment
    = reflectiveAttr "alignment" getter setter
    where
      getter w
        = do st <- get w style
             return (fromBitMask st)

      setter w align
        = set w [style :~ setBitMask align ]

-- | Widgets that have wrappable content.
class Wrapped w where
  -- | Set the wrap mode of a widget.
  wrap :: CreateAttr w Wrap

initialWrap cont props style
  = case filterProperty wrap props of
      (PropValue x, ps)  -> cont ps (setBitMask x style)
      (PropModify f, ps) -> cont ps (setBitMask (f (fromBitMask style)) style)
      (PropNone, ps)     -> cont ps style

instance Wrapped (TextCtrl a) where
  wrap
    = reflectiveAttr "wrap" getter setter
    where
      getter w
        = do st <- get w style
             return (fromBitMask st)

      setter w mode
        = set w [style :~ setBitMask mode]



{-
instance Able (TextCtrl a) where
  enabled
    = newAttr "enabled" textCtrlIsEditable textCtrlSetEditable
-}

-- Workaround for Unexpected TextCtrl behaviour (https://github.com/jodonoghue/wxHaskell/issues/1#issuecomment-5202439)
-- Problem arises from the fact that wxTE_RICH is needed only on Windows platforms, but is essential there. However, on
-- wxMac > 2.9, wxTE_RICH seems not to be ignored as the documentation claims.
getRichTE =  if (os == "mingw32") || (os == "win32")
             then wxTE_RICH
             else 0

getRichTE2 = if (os == "mingw32") || (os == "win32")
             then wxTE_RICH2
             else 0

-- | Create a single-line text entry control. Note: 'alignment' has to
-- be set at creation time (or the entry has default alignment (=left) ).
-- This is an alias for textEntry
--
-- * Instances: 'Wrap', 'Aligned', 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
entry :: Window a -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
entry parent props
  = textCtrlEx parent getRichTE props

-- | Create a single-line text entry control. Note: 'alignment' has to
-- be set at creation time (or the entry has default alignment (=left) ).
--
-- * Instances: 'Wrap', 'Aligned', 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
textEntry :: Window a -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textEntry parent props
  = textCtrlEx parent getRichTE props

-- | Create a multi-line text control. Note: the 'wrap' and 'alignment'
-- have to be set at creation time or the default to 'WrapNone' and 'AlignLeft' respectively.
--
-- * Instances: 'Wrap', 'Aligned', 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
textCtrl :: Window a -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrl parent props
  = textCtrlEx parent (wxTE_MULTILINE .+. getRichTE) props


-- | Create a multi-line text rich-text control with a certain wrap mode
-- Enables font and color settings on windows, while being equal to 'textCtrl'
-- on other platforms. Note: the 'wrap' and 'alignment'
-- have to be set at creation time or the default to 'WrapNone' and 'AlignLeft' respectively.
--
-- * Instances: 'Wrap', 'Aligned', 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
textCtrlRich :: Window a -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrlRich parent props
  = textCtrlEx parent (wxTE_MULTILINE .+. getRichTE2) props

-- | Create a generic text control given a certain style.
--
-- * Instances: 'Wrap', 'Aligned', 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
textCtrlEx :: Window a -> Style -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrlEx parent stl props
  = feed2 props stl $
    initialWindow    $ \id rect ->
    initialText      $ \txt ->
    initialWrap      $
    initialAlignment $ \props flags ->
    do e <- textCtrlCreate parent id txt rect flags
       set e props
       return e

-- | Complete the construction of a text control instance which has been loaded
--   from a resource file.
--
-- * Instances: 'Wrap', 'Aligned', 'Commanding' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
textCtrlRes :: Window a -> String -> [Prop (TextCtrl ())] -> IO (TextCtrl ())
textCtrlRes parent name props =
    do t <- xmlResourceGetTextCtrl parent name
       set t props
       return t

instance Commanding (TextCtrl a) where
  command = newEvent "command" textCtrlGetOnTextEnter textCtrlOnTextEnter

-- | Process @enter@ key events, used in a 'comboBox' or 'textCtrl' and
-- catched using a 'on' 'command' handler.
-- (otherwise pressing @Enter@ is either processed
-- internally by the control or used for navigation between dialog controls).
processEnter :: Styled w => Attr w Bool
processEnter
  = newAttr "processEnter" getter setter
  where
    getter w
      = do s <- get w style
           return (bitsSet wxTE_PROCESS_ENTER s)
    setter w p
      = set w [style :~ \stl -> stl .+. wxTE_PROCESS_ENTER]


-- | Process @tab@ key events, used in a 'comboBox' or 'textCtrl'.
-- (otherwise pressing @Tab@ is either processed
-- internally by the control or used for navigation between dialog controls).
processTab :: Styled w => Attr w Bool
processTab
  = newAttr "processTab" getter setter
  where
    getter w
      = do s <- get w style
           return (bitsSet wxTE_PROCESS_TAB s)
    setter w p
      = set w [style :~ \stl -> stl .+. wxTE_PROCESS_TAB]


{--------------------------------------------------------------------------------
  Static text
--------------------------------------------------------------------------------}
-- | Create static text label, see also 'label'.
staticText :: Window a -> [Prop (StaticText ())] -> IO (StaticText ())
staticText parent props
  = feed2 props 0 $
    initialWindow $ \id rect ->
    initialText   $ \txt -> \props flags ->
    do t <- staticTextCreate parent id txt rect flags {- (wxALIGN_LEFT + wxST_NO_AUTORESIZE) -}
       set t props
       return t

-- | Complete the construction of a static text label instance which has been loaded
--   from a resource file.
staticTextRes :: Window a -> String -> [Prop (StaticText ())] -> IO (StaticText ())
staticTextRes parent name props =
    do t <- xmlResourceGetStaticText parent name
       set t props
       return t

{--------------------------------------------------------------------------------
  Check box
--------------------------------------------------------------------------------}
instance Commanding (CheckBox a) where
  command = newEvent "command" checkBoxGetOnCommand checkBoxOnCommand

instance Checkable (CheckBox a) where
  checkable
    = enabled

  checked
    = newAttr "checked" checkBoxGetValue checkBoxSetValue

-- | Create a new checkbox.
--
-- * Instances: 'Commanding','Checkable' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
checkBox :: Window a -> [Prop (CheckBox ())] -> IO (CheckBox ())
checkBox parent props
  = feed2 props 0 $
    initialWindow $ \id rect ->
    initialText   $ \txt -> \props flags ->
    do c <- checkBoxCreate parent id txt rect flags
       set c props
       return c

-- | Complete the construction of a check box instance which has been loaded
--   from a resource file.
--
-- * Instances: 'Commanding','Checkable' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
checkBoxRes :: Window a -> String -> [Prop (CheckBox ())] -> IO (CheckBox ())
checkBoxRes parent name props =
    do c <- xmlResourceGetCheckBox parent name
       set c props
       return c

{--------------------------------------------------------------------------------
  Choice
--------------------------------------------------------------------------------}
-- | Widgets that have sorted contents.
class Sorted w where
  -- | Is the content of the widget sorted?
  sorted :: CreateAttr w Bool

instance Sorted (Choice a) where
  sorted
    = createAttr "sorted" getter setter
    where
      getter w
        = do st <- get w style
             return (bitsSet wxCB_SORT st)
      setter w sort
        = set w [style :~ \st -> if sort then st .+. wxCB_SORT else st .-. wxCB_SORT]

initialSorted :: Sorted w => ([Prop w] -> Style -> a) -> [Prop w] -> Style -> a
initialSorted
  = withStyleProperty sorted wxCB_SORT


instance Selecting (Choice ()) where
  select = newEvent "select" choiceGetOnCommand choiceOnCommand

instance Selection (Choice ()) where
  selection
    = newAttr "selection" choiceGetSelection choiceSetSelection


instance Items (Choice a) String where
  itemCount
    = readAttr "itemCount" choiceGetCount

  item i
    = newAttr "item" (\w -> choiceGetString w i) (\w x -> choiceSetString w i x)

  itemAppend w x
    = choiceAppend w x

  itemDelete w i
    = choiceDelete w i


-- | Create a choice item to select a one of a list of strings.
--
-- * Instances: 'Sorted', 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
choice :: Window a -> [Prop (Choice ())] -> IO (Choice ())
choice parent props
  = choiceEx parent 0 props


-- | Create a choice item, given a set of style flags, to select a one of a list of strings
--
-- * Instances: 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
choiceEx :: Window a -> Style -> [Prop (Choice ())] -> IO (Choice ())
choiceEx parent flags props
  = feed2 props flags $
    initialWindow $ \id rect ->
    initialSorted $ \props flags ->
    do c <- choiceCreate parent id rect [] flags
       set c props
       return c

-- | Complete the construction of a choice instance which has been loaded
--   from a resource file.
choiceRes :: Window a -> String -> [Prop (Choice ())] -> IO (Choice ())
choiceRes parent name props =
    do c <- xmlResourceGetChoice parent name
       set c props
       return c

{--------------------------------------------------------------------------------
  ComboBox
--------------------------------------------------------------------------------}
instance Commanding (ComboBox a) where
  command
    = newEvent "command" comboBoxGetOnTextEnter comboBoxOnTextEnter

instance Selecting (ComboBox a) where
  select
    = newEvent "select" comboBoxGetOnCommand comboBoxOnCommand

instance Selection (ComboBox a) where
  selection
    = newAttr "selection" comboBoxGetSelection comboBoxSetSelection


-- implemented by choice
{-
instance Items (ComboBox a) String where
  itemCount
    = readAttr "itemCount" comboBoxGetCount

  item i
    = readAttr "item" (\w -> comboBoxGetString w i) -- (\w x -> comboBoxSetString w i x)

  itemAppend w x
    = comboBoxAppend w x

  itemDelete w i
    = comboBoxDelete w i
-}

-- | Create a new combo box.
--
-- * Instances: 'Selecting', 'Commanding','Selection','Items' -- 'Textual', 'Literate', 'Dimensions',
--              'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
-- A 'command' event is triggered when the @enter@ key is pressed and when
-- 'processEnter' has been set to 'True'.
comboBox :: Window a -> [Prop (ComboBox ())] -> IO (ComboBox ())
comboBox parent props
  = comboBoxEx parent (wxCB_DROPDOWN) props


-- | Create a new combo box with a given set of flags.
--
-- * Instances: 'Selecting', 'Commanding','Selection','Items' -- 'Textual', 'Literate', 'Dimensions',
--              'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
-- A 'command' event is triggered when the @enter@ key is pressed and when
-- 'processEnter' has been set to 'True'.
comboBoxEx :: Window a -> Style -> [Prop (ComboBox ())] -> IO (ComboBox ())
comboBoxEx parent flags props
  = feed2 props flags $
    initialWindow $ \id rect ->
    initialText   $ \txt ->
    initialSorted $ \props flags ->
    do cb <- comboBoxCreate parent id txt rect [] flags
       set cb props
       return cb

-- | Complete the construction of a combo box instance which has been loaded
--   from a resource file.
comboBoxRes :: Window a -> String -> [Prop (ComboBox ())] -> IO (ComboBox ())
comboBoxRes parent name props =
    do c <- xmlResourceGetComboBox parent name
       set c props
       return c

{--------------------------------------------------------------------------------
  ListBox
--------------------------------------------------------------------------------}
instance Sorted (ListBox a) where
  sorted
    = createAttr "sorted" getter setter
    where
      getter w
        = do st <- get w style
             return (bitsSet wxLB_SORT st)
      setter w sort
        = set w [style :~ \st -> if sort then st .+. wxLB_SORT else st .-. wxLB_SORT]



instance Selecting (ListBox a) where
  select
    = newEvent "select" listBoxGetOnCommand listBoxOnCommand

instance Items (ListBox a) String where
  itemCount
    = readAttr "itemCount" listBoxGetCount

  item i
    = newAttr "item" (\w -> listBoxGetString w i) (\w x -> listBoxSetString w i x)

  itemAppend w x
    = listBoxAppend w x

  itemDelete w i
    = listBoxDelete w i

-- | Pointer to single selection list boxes, deriving from 'ListBox'.
type SingleListBox a  = ListBox (CSingleListBox a)

-- | Abstract type of the 'SingleListBox' class.
data CSingleListBox a = CSingleListBox

instance Selection (SingleListBox a) where
  selection
    = newAttr "selection" listBoxGetSelection (\w x -> listBoxSetSelection w x True)


-- | Pointer to multiple selection list boxes, deriving from 'ListBox'.
type MultiListBox a   = ListBox (CMultiListBox a)

-- | Abstract type of the 'MultiListBox' class.
data CMultiListBox a  = CMultiListBox           

instance Selections (MultiListBox a) where
  selections
    = newAttr "selections" listBoxGetSelectionList setter
    where
      setter w is =
        do oldSelection <- listBoxGetSelectionList w
           sequence_ [ listBoxSetSelection w i False -- deselect old selections
                     | i <- oldSelection
                     , i `notElem` is -- but not the ones in the new selection
                     ]
           mapM_ (\i -> listBoxSetSelection w i True) is


-- | Create a single selection list box.
--
-- * Instances: 'Sorted','Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
singleListBox :: Window a -> [Prop (SingleListBox ())] -> IO (SingleListBox ())
singleListBox parent props
  = feed2 props (wxLB_SINGLE .+. wxHSCROLL .+. wxLB_NEEDED_SB) $
    initialWindow $ \id rect ->
    initialSorted $ \props flags ->
    do lb <- listBoxCreate parent id rect [] flags
       let sl = (objectCast lb :: SingleListBox ())
       set sl props
       return sl

-- | Complete the construction of a single list box instance which has been loaded
--   from a resource file.
singleListBoxRes :: Window a -> String -> [Prop (SingleListBox ())] -> IO (SingleListBox ())
singleListBoxRes parent name props =
    do l <- xmlResourceGetListBox parent name
       let sl = (objectCast l :: SingleListBox())
       set sl props
       return sl

-- | Create a multi selection list box.
----
-- * Instances: 'Sorted', 'Selecting','Selections','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
multiListBox :: Window a -> [Prop (MultiListBox ())] -> IO (MultiListBox ())
multiListBox parent props
  = feed2 props (wxLB_MULTIPLE .+. wxLB_EXTENDED .+. wxHSCROLL .+. wxLB_NEEDED_SB) $
    initialWindow $ \id rect ->
    initialSorted $ \props flags ->
    do lb <- listBoxCreate parent id rect [] flags
       let ml = (objectCast lb :: MultiListBox ())
       set ml props
       return ml

-- | Complete the construction of a single list box instance which has been loaded
--   from a resource file.
multiListBoxRes :: Window a -> String -> [Prop (MultiListBox ())] -> IO (MultiListBox ())
multiListBoxRes parent name props =
    do l <- xmlResourceGetListBox parent name
       let ml = (objectCast l :: MultiListBox())
       set ml props
       return ml

{--------------------------------------------------------------------------------
  RadioBox
--------------------------------------------------------------------------------}
instance Selecting (RadioBox a) where
  select
    = newEvent "select" radioBoxGetOnCommand radioBoxOnCommand

instance Selection (RadioBox a) where
  selection
    = newAttr "selection" radioBoxGetSelection radioBoxSetSelection

instance Items (RadioBox a) String where
  itemCount
    = readAttr "itemCount" radioBoxNumber

  item i
    = newAttr "item" (\r -> radioBoxGetItemLabel r i) (\r s -> radioBoxSetItemLabel r i s)

  itemAppend
    = error "Controls.itemAppend: you can not append items to a radiobox"

  itemDelete
    = error "Controls.itemDelete: you can not delete items of a radiobox"


-- | Create a new radio button group with an initial orientation and a list of
-- labels. Use 'selection' to get the currently selected item.
--
-- * Instances: 'Selecting','Selection','Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
radioBox :: Window a -> Orientation -> [String] -> [Prop (RadioBox ())] -> IO (RadioBox ())
radioBox parent direction labels props
  = feed2 props (if (direction==Horizontal) then wxRA_SPECIFY_ROWS else wxRA_SPECIFY_COLS) $
    initialWindow $ \id rect ->
    initialText   $ \title -> \props flags ->
    do r <- radioBoxCreate parent id title rect labels 1 flags
       set r props
       return r

-- | Complete the construction of a radio box instance which has been loaded
--   from a resource file.
radioBoxRes :: Window a -> String -> [Prop (RadioBox ())] -> IO (RadioBox ())
radioBoxRes parent name props =
    do rb <- xmlResourceGetRadioBox parent name
       set rb props
       return rb

{--------------------------------------------------------------------------------
  Gauge
--------------------------------------------------------------------------------}
-- | Create a horizontal gauge with a specified integer range (max value).
-- The 'selection' attribute determines the position of the gauge.
--
-- * Instances: 'Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
hgauge :: Window a -> Int -> [Prop (Gauge ())] -> IO (Gauge ())
hgauge parent range props
  = gaugeEx parent range (wxHORIZONTAL .+. wxGA_SMOOTH) props

-- | Create a vertical gauge with a specified integer range (max value).
-- The 'selection' attribute determines the position of the gauge.
--
-- * Instances: 'Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
vgauge :: Window a -> Int -> [Prop (Gauge ())] -> IO (Gauge ())
vgauge parent range props
  = gaugeEx parent range (wxVERTICAL .+. wxGA_SMOOTH) props

-- | Create a gauge control.
-- The 'selection' attribute determines the position of the gauge.
--
-- * Instances: 'Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
gaugeEx :: Window a -> Int -> Style -> [Prop (Gauge ())] -> IO (Gauge ())
gaugeEx parent range style props
  = do g <- gaugeCreate parent idAny range rectNull style
       set g props
       return g

-- | Complete the construction of a gauge instance which has been loaded
--   from a resource file.
gaugeRes :: Window a -> String -> [Prop (Gauge ())] -> IO (Gauge ())
gaugeRes parent name props =
    do g <- xmlResourceGetGauge parent name
       set g props
       return g

instance Selection (Gauge a) where
  selection
    = newAttr "selection" getter setter
    where
      getter g
        = do i  <- gaugeGetValue g
             hi <- gaugeGetRange g
             return (max 0 (min hi i))

      setter g i
        = do hi <- gaugeGetRange g
             gaugeSetValue g (max 0 (min hi i))

{--------------------------------------------------------------------------------
  Slider
--------------------------------------------------------------------------------}
instance Commanding (Slider a) where
  command = newEvent "command" sliderGetOnCommand sliderOnCommand

-- | Create a horizontal slider with a specified minimum and maximum. Set
-- the 'Bool' argument to 'True' to show labels (minimumn, maximum, and
-- current value). The 'selection' attribute gives the current value.
--
-- * Instances: 'Commanding','Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
hslider :: Window a -> Bool -> Int -> Int -> [Prop (Slider ())] -> IO (Slider ())
hslider parent showLabels min max props
  = sliderEx parent min max (wxHORIZONTAL .+. (if showLabels then wxSL_LABELS else 0)) props

-- | Create a vertical slider with a specified minimum and maximum. Set
-- the 'Bool' argument to 'True' to show labels (minimumn, maximum, and
-- current value). The 'selection' attribute gives the current value.
--
-- * Instances: 'Commanding','Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
vslider :: Window a -> Bool -> Int -> Int -> [Prop (Slider ())] -> IO (Slider ())
vslider parent showLabels min max props
  = sliderEx parent min max (wxVERTICAL .+. (if showLabels then wxSL_LABELS else 0)) props

-- | Create a slider with a specified minimum and maximum. The
-- 'selection' attribute gives the current value.
--
-- * Instances: 'Commanding','Selection' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
sliderEx :: Window a -> Int -> Int -> Style -> [Prop (Slider ())] -> IO (Slider ())
sliderEx parent min max style props
  = do s <- sliderCreate parent style min min max rectNull style
       set s props
       return s

-- | Complete the construction of a slider instance which has been loaded
--   from a resource file.
sliderRes :: Window a -> String -> [Prop (Slider ())] -> IO (Slider ())
sliderRes parent name props =
    do s <- xmlResourceGetSlider parent name
       set s props
       return s

instance Selection (Slider a) where
  selection
    = newAttr "selection" getter setter
    where
      getter s
        = do i  <- sliderGetValue s
             lo <- sliderGetMin s
             hi <- sliderGetMax s
             return (max lo (min hi i))

      setter s i
        = do lo <- sliderGetMin s
             hi <- sliderGetMax s
             sliderSetValue s (max lo (min hi i))


{--------------------------------------------------------------------------------
  SpinCtrl
--------------------------------------------------------------------------------}
-- | Create a spin control: a text field with up\/down buttons. The value ('selection')
-- is always between a specified minimum and maximum.
--
-- * Instances: 'Selection', 'Selecting' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child', 
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
spinCtrl :: Window a -> Int -> Int -> [Prop (SpinCtrl ())] -> IO (SpinCtrl ())
spinCtrl parent lo hi props
  = feed2 props wxSP_ARROW_KEYS $
    initialWindow $ \id rect ->
    initialText   $ \txt -> \props flags ->
    do sc <- spinCtrlCreate parent id txt rect flags (min lo hi) (max lo hi) lo
       set sc props
       return sc

-- | Complete the construction of a spin control instance which has been loaded
--   from a resource file.
spinCtrlRes :: Window a -> String -> [Prop (SpinCtrl ())] -> IO (SpinCtrl ())
spinCtrlRes parent name props =
    do s <- xmlResourceGetSpinCtrl parent name
       set s props
       return s


instance Selection (SpinCtrl a) where
  selection
    = newAttr "selection" getter setter
    where
      getter sc
        = do i  <- spinCtrlGetValue sc
             lo <- spinCtrlGetMin sc
             hi <- spinCtrlGetMax sc
             return (max lo (min hi i))

      setter sc i
        = do lo <- spinCtrlGetMin sc
             hi <- spinCtrlGetMax sc
             spinCtrlSetValue sc (max lo (min hi i))


instance Selecting (SpinCtrl a) where
  select 
    = newEvent "select" spinCtrlGetOnCommand spinCtrlOnCommand

{--------------------------------------------------------------------------------
  ToggleButton
--------------------------------------------------------------------------------}
-- | Create a toggle button. 
--
-- * Instances: 'Commanding',  -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
toggleButton :: Window a -> [Prop (ToggleButton ())] -> IO (ToggleButton ())
toggleButton parent props
  = feed2 props defaultStyle $
    initialWindow $ \id rect -> \props flags ->
    do bb <- toggleButtonCreate parent id "" rect flags
       set bb props
       return bb

instance Commanding (ToggleButton a) where
  command  = newEvent "command" toggleButtonGetOnCommand toggleButtonOnCommand

instance Checkable (ToggleButton a) where
  checkable = enabled 
  checked   = newAttr "checked" toggleButtonGetValue toggleButtonSetValue

-- | Create a bitmap toggle button. Use the 'picture' attribute to set the
-- bitmap.
--
-- * Instances: 'Commanding', 'Pictured' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
bitmapToggleButton :: Window a -> [Prop (BitmapToggleButton ())] -> IO (BitmapToggleButton ())
bitmapToggleButton parent props
  = feed2 props defaultStyle $
    initialWindow $ \id rect -> \props flags ->
    do bb <- bitmapToggleButtonCreate parent id nullBitmap rect flags
       set bb props
       return bb

instance Pictured (BitmapToggleButton a) where
  picture
    = writeAttr "picture" setter
    where
      setter w fname
        = do fpath <- getAbsoluteFilePath fname
             withBitmapFromFile fpath (bitmapToggleButtonSetBitmapLabel w)


{--------------------------------------------------------------------------------
  TreeCtrl
--------------------------------------------------------------------------------}
-- | Tree control events.
treeEvent :: Event (TreeCtrl a) (EventTree -> IO ())
treeEvent
  = newEvent "treeEvent" treeCtrlGetOnTreeEvent treeCtrlOnTreeEvent

-- | Create a single-selection tree control with buttons (i.e. + and - signs).
--
-- * Attributes: 'treeEvent'
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
treeCtrl :: Window a -> [Prop (TreeCtrl ())] -> IO (TreeCtrl ())
treeCtrl parent props
  = treeCtrlEx parent (wxTR_HAS_BUTTONS .+. defaultStyle) props

-- | Create a tree control.
--
-- * Attributes: 'treeEvent'
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
treeCtrlEx :: Window a -> Style -> [Prop (TreeCtrl ())] -> IO (TreeCtrl ())
treeCtrlEx parent style props
  = feed2 props style $
    initialContainer $ \id rect -> \props flags ->
    do t <- treeCtrlCreate2 parent id rect flags
       set t props
       return t

-- | Complete the construction of a tree control instance which has been loaded
--   from a resource file.
treeCtrlRes :: Window a -> String -> [Prop (TreeCtrl ())] -> IO (TreeCtrl ())
treeCtrlRes parent name props =
    do t <- xmlResourceGetTreeCtrl parent name
       set t props
       return t

{--------------------------------------------------------------------------------
  ListCtrl
--------------------------------------------------------------------------------}
instance Items (ListCtrl a) [String] where
  itemCount
    = readAttr "itemCount" listCtrlGetItemCount

  item i
    = newAttr "item" getter setter
    where
      getter l
        = bracket listItemCreate
                  listItemDelete
                  (\li -> do count <- listCtrlGetColumnCount l
                             mapM (\column -> do listItemSetColumn li (column-1)
                                                 listItemSetId li i
                                                 listItemSetMask li wxLIST_MASK_TEXT
                                                 listCtrlGetItem l li
                                                 listItemGetText li) [1..count])


      setter l texts
        = do count <- listCtrlGetItemCount l
             when (i == count) (do listCtrlInsertItemWithLabel l i (show i) (-1); return ())
             mapM_ (\(column,txt) -> listCtrlSetItem l i column txt (-1)) (zip [0..] texts)

  itemAppend l texts
    = do count <- listCtrlGetItemCount l
         listCtrlInsertItemWithLabel l count (show count) (-1)
         mapM_ (\(column,txt) -> listCtrlSetItem l count column txt (-1)) (zip [0..] texts)

  itemDelete l i
    = do listCtrlDeleteItem l i
         return ()

  itemsDelete l
    = do listCtrlDeleteAllItems l
         return ()

-- | The @columns@ attribute controls the columns in a report-view list control.
columns :: Attr (ListCtrl a) [(String,Align,Int)]
columns
  = newAttr "columns" getter setter
  where
    setter l xs
      = do n <- listCtrlGetColumnCount l
           mapM_ (\c -> listCtrlDeleteColumn l 0) (reverse [1..n])
           mapM_ (insertColumn l) (zip [0..] xs)
      where
        insertColumn l (idx,(name,align,width))
          = let alignment = case align of
                              AlignRight -> wxLIST_FORMAT_RIGHT
                              AlignCentre-> wxLIST_FORMAT_CENTER
                              other      -> wxLIST_FORMAT_LEFT
            in listCtrlInsertColumn l idx name alignment width

    getter l
      = do n <- listCtrlGetColumnCount l
           mapM (getColumn l) [0..n]
      where
        getColumn l idx
          = bracket (listCtrlGetColumn2 l idx)
                    (listItemDelete)
                    (\item -> do name      <- listItemGetText item
                                 alignment <- listItemGetAlign item
                                 width     <- listItemGetWidth item
                                 let align | alignment == wxLIST_FORMAT_RIGHT  = AlignRight
                                           | alignment == wxLIST_FORMAT_CENTER = AlignCentre
                                           | otherwise                         = AlignLeft
                                 return (name,align,width)
                    )



-- | List control events.
listEvent :: Event (ListCtrl a) (EventList -> IO ())
listEvent
  = newEvent "listEvent" listCtrlGetOnListEvent listCtrlOnListEvent

-- | Create a report-style list control.
--
-- * Attributes: 'listEvent', 'columns'
--
-- * Instances: 'Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
listCtrl :: Window a -> [Prop (ListCtrl ())] -> IO (ListCtrl ())
listCtrl parent props
  = listCtrlEx parent (wxLC_REPORT .+. defaultStyle) props

-- | Create a list control.
--
-- * Attributes: 'listEvent', 'columns'
--
-- * Instances: 'Items' -- 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
listCtrlEx :: Window a -> Style -> [Prop (ListCtrl ())] -> IO (ListCtrl ())
listCtrlEx parent style props
  = feed2 props style $
    initialContainer $ \id rect -> \props flags ->
    do l <- listCtrlCreate parent id rect flags
       set l props
       return l

-- | Complete the construction of a list control instance which has been loaded
--   from a resource file.
listCtrlRes :: Window a -> String -> [Prop (ListCtrl ())] -> IO (ListCtrl ())
listCtrlRes parent name props =
    do l <- xmlResourceGetListCtrl parent name
       set l props
       return l

-- TODO: figure out how to set a resize handler in addition to the
-- default one. this will allow us to expand as needed upon resize
-- events.
listCtrlSetColumnWidths :: ListCtrl () -> Int -> IO ()
listCtrlSetColumnWidths ctrl w = do
--  size <- (realToFrac.sizeW) `fmap` get ctrl clientSize
  cols <- listCtrlGetColumnCount ctrl
--  let w = 65 --ceiling $ size / realToFrac cols
  forM_ [0 .. cols - 1] $ \i -> listCtrlSetColumnWidth ctrl i w

-- | A small wrapper over WX's ListCtrl, allowing us to keep the data
--   we're representing as well as its string form (shown to the user as
--   rows).
data ListView a = ListView {
  listViewCtrl  :: ListCtrl (),
  listViewItems :: Var [a],
  listViewToRow :: a -> [String]
}

listViewLayout :: ListView a -> Layout
listViewLayout = fill . widget . listViewCtrl

listViewSetHandler :: ListView a -> (EventList -> IO ()) -> IO ()
listViewSetHandler list handler =
  set (listViewCtrl list) [on listEvent := handler]

listViewSelectHandle :: ListView a -> (Maybe a -> IO ()) -> EventList -> IO ()
listViewSelectHandle _    _   (ListItemActivated (-1)) = propagateEvent
listViewSelectHandle list end (ListItemActivated   n ) = end . Just =<< (!! n) `fmap` listViewGetItems list
listViewSelectHandle _    _   _                        = propagateEvent

listViewSetItems :: ListView a -> [a] -> IO ()
listViewSetItems list its = do
  set (listViewItems list) [value := its]
  set (listViewCtrl list)  [items := map (listViewToRow list) its]

listViewGetItems :: ListView a -> IO [a]
listViewGetItems list = get (listViewItems list) value

listViewAddItem :: ListView a -> a -> IO ()
listViewAddItem list it = do
  its <- (it:) `fmap` get (listViewItems list) value
  listViewSetItems list its

listViewSetColumnWidths :: ListView a -> Int -> IO ()
listViewSetColumnWidths list w = do
  listCtrlSetColumnWidths (listViewCtrl list) w

listView :: Window b -> [String] -> (a -> [String]) -> IO (ListView a)
listView parent cols toRow = do
  ctrl <- listCtrl parent [columns := map (\n -> (n, AlignLeft, -1)) cols]
  var  <- variable [value := []]
  return $ ListView ctrl var toRow

{--------------------------------------------------------------------------------
  SplitterWindow
--------------------------------------------------------------------------------}
-- | Create a splitter window.
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
splitterWindow :: Window a -> [Prop (SplitterWindow ())] -> IO (SplitterWindow ())
splitterWindow parent props
  = feed2 props (defaultStyle .+. wxSP_LIVE_UPDATE) $
    initialContainer $ \id rect -> \props flags ->
    do s <- splitterWindowCreate parent id rect flags
       set s props
       return s

{--------------------------------------------------------------------------------
  ImageList
--------------------------------------------------------------------------------}
-- | Create an empty image list that will contain images of the desired size.
imageList :: Size -> IO (ImageList ())
imageList size
  = imageListCreate size True 10

-- | Create an image list containing the images in the supplied file name list
-- that will be scaled towards the desired size.
imageListFromFiles :: Size -> [FilePath] -> IO (ImageList ())
imageListFromFiles size files
  = do images <- imageListCreate size True (length files)
       imageListAddIconsFromFiles images size files
       return images

{--------------------------------------------------------------------------------
  MediaCtrl
--------------------------------------------------------------------------------}

-- | Optional back-end for your MediaCtrl.
--   If you want to know more about back-end, you must see wxWidgets' Document.
--   <http://www.wxwidgets.org/manuals/stable/wx_wxmediactrl.html#choosingbackendwxmediactrl>
data MediaCtrlBackend =
      DirectShow            -- ^ Use ActiveMovie\/DirectShow. Default back-end on Windows.
    | MediaControlInterface -- ^ Use Media Command Interface. Windows Only.
    | WindowsMediaPlayer10  -- ^ Use Windows Media Player 10. Windows Only. Require to use wxWidgets 2.8.x.
    | QuickTime             -- ^ Use QuickTime. Mac Only. 
    | GStreamer             -- ^ Use GStreamer. Unix Only. Require GStreamer and GStreamer Support.
    | DefaultBackend        -- ^ Use default back-end on your platform.
   deriving (Eq,Show)

fromMediaCtrlBackend :: MediaCtrlBackend -> String
fromMediaCtrlBackend back
  = case back of
      DirectShow            -> wxMEDIABACKEND_DIRECTSHOW
      MediaControlInterface -> wxMEDIABACKEND_MCI
      WindowsMediaPlayer10  -> wxMEDIABACKEND_WMP10
      QuickTime             -> wxMEDIABACKEND_QUICKTIME
      GStreamer             -> wxMEDIABACKEND_GSTREAMER
      DefaultBackend        -> ""

-- FIXME: Change wxDirect to Support STRING type in Eiffel file (*.e)
-- instead of write definition directory here.
wxMEDIABACKEND_DIRECTSHOW = "wxAMMediaBackend"
wxMEDIABACKEND_MCI = "wxMCIMediaBackend"
wxMEDIABACKEND_WMP10 = "wxWMP10MediaBackend"
wxMEDIABACKEND_QUICKTIME = "wxQTMediaBackend"
wxMEDIABACKEND_GSTREAMER = "wxGStreamerMediaBackend"

mediaCtrl :: Window a -> [Prop (MediaCtrl ())] -> IO (MediaCtrl ())
mediaCtrl parent props
  = mediaCtrlEx parent defaultStyle DefaultBackend props

-- | Create MediaCtrl with choosing back-end. This is useful to select back-end on
-- Windows. But if you don't want to cause any effect to other platforms, you must
-- use wxToolkit or #ifdef macro to choose correct function for platforms.
-- For example,
--
-- > import Graphics.UI.WXCore.Defines
-- > ...
-- >   m <- case wxToolkit of
-- >          WxMSW -> mediaCtrlWithBackend f MediaControlInterface []
-- >          _     -> mediaCtrl f []
--
-- or
--
-- > #ifdef mingw32_HOST_OS || mingw32_TARGET_OS
-- >   m <-  mediaCtrlWithBackend f MediaControlInterface []
-- > #else
-- >   m <-  mediaCtrl f []
-- > #endif
--
mediaCtrlWithBackend :: Window a -> MediaCtrlBackend -> [Prop (MediaCtrl ())] -> IO (MediaCtrl ())
mediaCtrlWithBackend parent back props
  = mediaCtrlEx parent defaultStyle back props

mediaCtrlEx :: Window a -> Style -> MediaCtrlBackend -> [Prop (MediaCtrl ())] -> IO (MediaCtrl ())
mediaCtrlEx parent style back props
  = feed2 props style $
    initialContainer $ \id rect -> \props flags ->
    do s <- mediaCtrlCreate parent id "" rect style (fromMediaCtrlBackend back) ""
       set s props
       return s

instance Media (MediaCtrl a) where
  play media = unitIO (mediaCtrlPlay media)
  stop media = unitIO (mediaCtrlStop media)

{--------------------------------------------------------------------------------
  wxStyledTextCtrl
--------------------------------------------------------------------------------}

stcEvent :: Event (StyledTextCtrl ()) (EventSTC -> IO ())
stcEvent
  = newEvent "stcEvent" stcGetOnSTCEvent stcOnSTCEvent


styledTextCtrl :: Window a -> [Prop (StyledTextCtrl ())] -> IO (StyledTextCtrl ())
styledTextCtrl parent props
  = styledTextCtrlEx parent defaultStyle props

styledTextCtrlEx :: Window a -> Style -> [Prop (StyledTextCtrl ())] -> IO (StyledTextCtrl ())
styledTextCtrlEx parent style props
  = feed2 props style $
    initialContainer $ \id rect -> \props flags ->
    do s <- styledTextCtrlCreate parent id "" rect style
       set s props
       return s

{--------------------------------------------------------------------------------
  PropertyGrid
--------------------------------------------------------------------------------}

-- | PropertyGrid control events.
propertyGridEvent :: Event (PropertyGrid a) (EventPropertyGrid -> IO ())
propertyGridEvent
  = newEvent "propertyGridEvent" propertyGridGetOnPropertyGridEvent propertyGridOnPropertyGridEvent

-- | Create a property grid.
--
-- * Attributes: 'propertyGridEvent',
--
-- * Instances: 'Textual', 'Literate', 'Dimensions', 'Colored', 'Visible', 'Child',
--             'Able', 'Tipped', 'Identity', 'Styled', 'Reactive', 'Paint'.
--
propertyGrid :: Window a -> [Prop (PropertyGrid ())] -> IO (PropertyGrid ())
propertyGrid parent props
  = feed2 props wxPG_DEFAULT_STYLE $
    initialContainer $ \id rect -> \props flags ->
    do l <- propertyGridCreate parent id rect flags
       set l props
       return l

