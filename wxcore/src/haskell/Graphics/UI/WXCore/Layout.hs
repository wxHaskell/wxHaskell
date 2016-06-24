{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------------------
{-|
Module      :  Layout
Copyright   :  (c) Daan Leijen & Wijnand van Suijlen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Combinators to specify layout. (These combinators use wxWidgets 'Sizer' objects).

Layout can be specified using 'windowSetLayout'. For example:

> do f  <- frameCreateTopFrame "Test"
>    ok <- buttonCreate f idAny "Bye" rectNull 0
>    windowSetLayout f (widget ok)
>    ...

The 'windowSetLayout' function takes 'Layout' as its argument.
The 'widget' combinator creates a layout from a window. The 'space' combinator creates
an empty layout with a specific width and height. Furthermore, we have the 'label' combinator
to create a static label label and 'boxed' to create a labeled border around a layout.
The 'grid' combinator lays out elements in a table with a given space between the elements.
Here is for example a layout for retrieving an /x/ and /y/ coordinate from the user, with 5 pixels space
between the controls:

> boxed "coordinates" (grid 5 5 [[label "x", widget xinput]
>                               ,[label "y", widget yinput]])

Combinators like 'row' and 'column' are specific instances of grids. We can use
these combinator to good effect to add an /ok/ and /cancel/ button at the bottom of our dialog:

> column 5 [ boxed "coordinates" (grid 5 5 [[label "x", widget xinput]
>                                          ,[label "y", widget yinput]])
>          , row 5 [widget ok, widget cancel]]

Layout /tranformers/ influence attributes of a layout. The 'margin' combinator adds a
margin around a layout. The /align/ combinators specify how a combinator is aligned when
the assigned area is larger than the layout itself. We can use these transformers to
add a margin around our dialog and to align the buttons to the bottom right (instead of the
default top-left):

> margin 10 $ column 5 [ boxed "coordinates" (grid 5 5 [[label "x", widget xinput]
>                                                      ,[label "y", widget yinput]])
>                      , alignBottomRight $ row 5 [widget ok, widget cancel]]

Besides aligning a layout in its assigned area, we can also specify that a layout should
expand to fill the assigned area. The 'shaped' combinator fills an area while maintaining the
original proportions (or aspect ratio) of a layout. The 'expand' combinator always tries to fill
the entire area (and alignment is ignored).

The final attribute is the /stretch/ of a layout. A stretchable layout may get a larger
area assigned than the minimally required area. This can be used to fill out the entire parent
area -- this happens for example when a user enlarges a dialog.

The default stretch and expansion mode of layout containers, like 'grid' and 'boxed', depends on the
stretch of the child layouts.  A column of a /grid/ is only stretchable when all
elements of that column have horizontal stretch. The same holds for rows with vertical stretch.
When any column or row is stretchable, the grid itself will also be stretchable in that direction
and the grid will 'expand' to fill the assigned area by default (instead of being 'static'). Just like
a grid, other containers, like 'container', 'boxed', 'tabs', 'row', and 'column', will also propagate the stretch
and expansion mode of their child layouts.

Armed with the 'stretch' combinators we can make our dialog resizeable.
We let the input widgets resize horizontally. Furthermore, the button row will resize
vertically and horizontally with the buttons aligned to the bottom right. Due to the
stretch propagation rules, the 'grid' and 'boxed' stretch horizontally and 'expand' to fill the
assigned area. The horizontal 'row' does /not/ stretch by default (and we need to use
an explicit 'stretch') and it does /not/ expand into the assigned area by default (and therefore
alignment works properly).

> margin 10 $ column 5 [ boxed "coordinates" (grid 5 5 [[label "x", hstretch $ expand $ widget xinput]
>                                                      ,[label "y", hstretch $ expand $ widget yinput]])
>                      , stretch $ alignBottomRight $ row 5 [widget ok, widget cancel]]

There are some common uses of stretchable combinators. The 'fill' combinators combine
stretch and expansion. For example, 'hfill' is defined as (@hstretch . expand@). The /float/
combinators combine alignment and 'stretch'. For example, 'floatBottomRight' is defined
as (@stretch . alignBottomRight@). There are also horizontal and vertical float combinators,
like 'hfloatCentre' and 'vfloatBottom'. Here is the above example using 'fill' and float:

> margin 10 $ column 5 [ boxed "coordinates" (grid 5 5 [[label "x", hfill $ widget xinput]
>                                                      ,[label "y", hfill $ widget yinput]])
>                      , floatBottomRight $ row 5 [widget ok, widget cancel]]

The 'glue' combinators are stretchable empty space. For example, 'hglue'
is defined as (@hstretch (space 0 0)@). We can use glue to mimic alignment. Here is the above
layout specified with glue. Note that we use 'hspace' to manually insert
space between the elements or  otherwise there would be space between the glue and
the /ok/ button.

> margin 10 $ column 5 [ boxed "coordinates" (grid 5 5 [[label "x", hfill $ widget xinput]
>                                                      ,[label "y", hfill $ widget yinput]])
>                      , glue
>                      , row 0 [hglue, widget ok, hspace 5, widget cancel]]

Splitter windows can also be specified with layout; you get somewhat less functionality
but it is quite convenient for most applications. A horizontal split is done using
'hsplit' while a vertical split is specified with a 'vsplit'.

The layout for notebooks is specified with the 'tabs' combinator. The following
example shows this (and note also how we use 'container' to set the layout of panels):

> nbook  <- notebookCreate ...
> panel1 <- panelCreate nbook ...
> ...
> panel2 <- panelCreate nbook ...
> ...
> windowSetLayout frame
>    (tabs nbook [tab "page one" $ container panel1 $ margin 10 $ floatCentre $ widget ok
>                ,tab "page two" $ container panel2 $ margin 10 $ hfill $ widget quit])

The pages /always/ need to be embedded inside a 'container' (normally a 'Panel'). The
title of the pages is determined from the label of the container widget.

Note: /At the moment, extra space is divided evenly among stretchable layouts. We plan to add
a (/@proportion :: Int -> Layout -> Layout@/) combinator in the future to stretch layouts
according to a relative weight, but unfortunately, that entails implementing a better
/'FlexGrid'/ sizer for wxWidgets./
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Layout( -- * Types
                               Layout, sizerFromLayout
                             , TabPage
                               -- * Window
                             , windowSetLayout, layoutFromWindow
                             , windowReFit, windowReFitMinimal
                             , windowReLayout, windowReLayoutMinimal
                               -- * Layouts                               
                               -- ** Widgets
                             , Widget, widget, label, rule, hrule, vrule, sizer
                               -- ** Containers
                             , row, column
                             , grid, boxed, container, tab, imageTab, tabs
                             , hsplit, vsplit
                               -- ** Glue
                             , glue, hglue, vglue
                               -- ** Whitespace
                             , space, hspace, vspace, empty
                               -- * Transformers
                             , dynamic
                               -- ** Stretch
                             , static, stretch, hstretch, vstretch, minsize
                               -- ** Expansion
                             , rigid, shaped, expand
                               -- ** Fill
                             , fill, hfill, vfill
                               -- ** Margin
                             , margin, marginWidth, marginNone
                             , marginLeft, marginTop, marginRight, marginBottom
                               -- ** Floating alignment
                             , floatTopLeft, floatTop, floatTopRight
                             , floatLeft, floatCentre, floatCenter, floatRight
                             , floatBottomLeft, floatBottom, floatBottomRight
                               -- ** Horizontal floating alignment
                             , hfloatLeft, hfloatCentre, hfloatCenter, hfloatRight
                               -- ** Vertical floating alignment
                             , vfloatTop, vfloatCentre, vfloatCenter, vfloatBottom
                               -- ** Alignment
                             , center, centre
                             , alignTopLeft, alignTop, alignTopRight
                             , alignLeft, alignCentre, alignCenter, alignRight
                             , alignBottomLeft, alignBottom, alignBottomRight
                               -- ** Horizontal alignment
                             , halignLeft, halignCentre, halignCenter, halignRight
                               -- ** Vertical alignment
                             , valignTop, valignCentre, valignCenter, valignBottom
                             , nullLayouts -- This is just to remove "Defined but not used" warnings
                             ) where

import Data.List( transpose )
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.WxcClassInfo
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Frame

{-----------------------------------------------------------------------------------------
  Classes
-----------------------------------------------------------------------------------------}
-- | Anything in the widget class can be layed out.
class Widget w where
  -- | Create a layout from a widget.
  widget :: w -> Layout

instance Widget Layout where
  widget layout
    = layout

instance Widget (Window a) where
  widget w
    = layoutFromWindow w


{-----------------------------------------------------------------------------------------
  Abstractions
-----------------------------------------------------------------------------------------}
{-
-- | Layout elements horizontally with no space between the elements.
row :: [Layout] -> Layout
row
  = horizontal 0

-- | Layout elements vertically with no space between the elements.
column :: [Layout] -> Layout
column
  = vertical 0

-- | Layout elements in a 'grid' with no space between the elements.
matrix :: [[Layout]] -> Layout
matrix
  = grid 0 0

-- | Create a border. (= @'boxed' \"\"@).
border :: Layout -> Layout
border
  = boxed ""

-}

-- | The layout is stretchable and expands into the assigned area. (see also 'stretch' and 'expand').
fill :: Layout -> Layout
fill
  = stretch . expand

-- | The layout is horizontally stretchable and expands into the assigned area. (see also 'hstretch' and 'expand').
hfill :: Layout -> Layout
hfill
  = hstretch . expand

-- | The layout is vertically stretchable and expands into the assigned area. (see also 'vstretch' and 'expand').
vfill :: Layout -> Layout
vfill
  = vstretch . expand

-- | Layout elements in a horizontal direction with a certain amount of space between the elements.
row :: Int -> [Layout] -> Layout
row w row'
  = grid w 0 [row']

-- | Layout elements in a vertical direction with a certain amount of space between the elements.
column :: Int -> [Layout] -> Layout
column h col
  = grid 0 h (map (\x -> [x]) col)



{-----------------------------------------------------------------------------------------
  Float
-----------------------------------------------------------------------------------------}
-- | Make the layout stretchable and align it in the center of the assigned area.
floatCenter :: Layout -> Layout
floatCenter
  = floatCentre

-- | Make the layout stretchable and align it in the center of the assigned area.
floatCentre :: Layout -> Layout
floatCentre
  = stretch . alignCentre

-- | Make the layout stretchable and align it in the top-left corner of the assigned area (default).
floatTopLeft :: Layout -> Layout
floatTopLeft
  = stretch . alignTopLeft

-- | Make the layout stretchable and align it centered on the top of the assigned area.
floatTop :: Layout -> Layout
floatTop
  = stretch . alignTop

-- | Make the layout stretchable and align it to the top-right of the assigned area.
floatTopRight :: Layout -> Layout
floatTopRight
  = stretch . alignTopRight

-- | Make the layout stretchable and align it centered to the left of the assigned area.
floatLeft :: Layout -> Layout
floatLeft
  = stretch . alignLeft

-- | Make the layout stretchable and align it centered to the right of the assigned area.
floatRight :: Layout -> Layout
floatRight
  = stretch . alignRight

-- | Make the layout stretchable and align it to the bottom-left of the assigned area.
floatBottomLeft :: Layout -> Layout
floatBottomLeft
  = stretch . alignBottomLeft

-- | Make the layout stretchable and align it centered on the bottom of the assigned area.
floatBottom :: Layout -> Layout
floatBottom
  = stretch . alignBottom

-- | Make the layout stretchable and align it to the bottom-right of the assigned area.
floatBottomRight :: Layout -> Layout
floatBottomRight
  = stretch . alignBottomRight

-- | Make the layout horizontally stretchable and align to the center.
hfloatCenter :: Layout -> Layout
hfloatCenter 
  = hfloatCentre

-- | Make the layout horizontally stretchable and align to the center.
hfloatCentre :: Layout -> Layout
hfloatCentre
  = hstretch . alignCentre

-- | Make the layout horizontally stretchable and align to the left.
hfloatLeft :: Layout -> Layout
hfloatLeft
  = hstretch . alignLeft

-- | Make the layout horizontally stretchable and align to the right.
hfloatRight :: Layout -> Layout
hfloatRight
  = hstretch . alignRight

-- | Make the layout vertically stretchable and align to the center.
vfloatCenter :: Layout -> Layout
vfloatCenter
  = vfloatCentre

-- | Make the layout vertically stretchable and align to the center.
vfloatCentre :: Layout -> Layout
vfloatCentre
  = vstretch . alignCentre

-- | Make the layout vertically stretchable and align to the top.
vfloatTop :: Layout -> Layout
vfloatTop
  = vstretch . alignTop

-- | Make the layout vertically stretchable and align to the bottom.
vfloatBottom :: Layout -> Layout
vfloatBottom
  = vstretch . alignBottom

{-----------------------------------------------------------------------------------------
  Alignment
-----------------------------------------------------------------------------------------}
-- | Align the layout in the center of the assigned area.
center :: Layout -> Layout
center
  = centre

-- | Align the layout in the center of the assigned area.
centre :: Layout -> Layout
centre
  = alignCentre

-- | Align the layout in the center of the assigned area.
alignCenter :: Layout -> Layout
alignCenter
  = alignCentre

-- | Align the layout in the center of the assigned area.
alignCentre :: Layout -> Layout
alignCentre
  = halignCentre . valignCentre

-- | Align the layout in the top-left corner of the assigned area (default).
alignTopLeft :: Layout -> Layout
alignTopLeft
  = valignTop . halignLeft

-- | Align the layout centered on the top of the assigned area.
alignTop :: Layout -> Layout
alignTop
  = valignTop . halignCentre

-- | Align the layout to the top-right of the assigned area.
alignTopRight :: Layout -> Layout
alignTopRight
  = valignTop . halignRight

-- | Align the layout centered to the left of the assigned area.
alignLeft :: Layout -> Layout
alignLeft
  = valignCentre . halignLeft

-- | Align the layout centered to the right of the assigned area.
alignRight :: Layout -> Layout
alignRight
  = valignCentre . halignRight

-- | Align the layout to the bottom-left of the assigned area.
alignBottomLeft :: Layout -> Layout
alignBottomLeft
  = valignBottom . halignLeft

-- | Align the layout centered on the bottom of the assigned area.
alignBottom :: Layout -> Layout
alignBottom
  = valignBottom . halignCentre

-- | Align the layout to the bottom-right of the assigned area.
alignBottomRight :: Layout -> Layout
alignBottomRight
  = valignBottom . halignRight

{-----------------------------------------------------------------------------------------
  Whitespace
-----------------------------------------------------------------------------------------}

-- | An empty layout that stretchable in all directions.
glue :: Layout
glue
  = stretch empty

-- | An empty layout that is vertically stretchable.
vglue :: Layout
vglue
  = vstretch empty

-- | An empty layout that is horizontally stretchable.
hglue :: Layout
hglue
  = hstretch empty

-- | An empty layout. (see also 'space').
empty :: Layout
empty
  = space 0 0

-- | Horizontal 'space' of a certain width.
hspace :: Int -> Layout
hspace w
  = space w 0

-- | Vertical 'space' of a certain height.
vspace :: Int -> Layout
vspace h
  = space 0 h


{-----------------------------------------------------------------------------------------
  Primitive layout tranformers
-----------------------------------------------------------------------------------------}

-- | (primitive) Set the minimal size of a widget.
minsize :: Size -> Layout -> Layout
minsize sz' layout
  = updateOptions layout (\options' -> options'{ minSize = Just sz' })

-- | (primitive) Never resize the layout, but align it in the assigned area
-- (default, except for containers like 'grid' and 'boxed' where it depends on the child layouts).
rigid :: Layout -> Layout
rigid layout
  = updateOptions layout (\options' -> options'{ fillMode = FillNone })

-- | (primitive) Expand the layout to fill the assigned area but maintain the original proportions
-- of the layout. Note that the layout can still be aligned in a horizontal or vertical direction.
shaped :: Layout -> Layout
shaped layout
  = updateOptions layout (\options' -> options'{ fillMode = FillShaped })

-- | (primitive) Expand the layout to fill the assigned area entirely, even when the original proportions can not
-- be maintained. Note that alignment will have no effect on such layout. See also 'fill'.
expand :: Layout -> Layout
expand layout
  = updateOptions layout (\options' -> options'{ fillMode = Fill })


-- | (primitive) The layout is not stretchable. In a 'grid', the row and column that contain this layout will
-- not be resizeable. Note that a 'static' layout can still be assigned an area that is larger
-- than its preferred size due to grid alignment constraints.
-- (default, except for containers like 'grid' and 'boxed' where it depends on the child layouts).
static :: Layout -> Layout
static layout
  = updateOptions layout (\options' -> options'{ stretchV = False, stretchH = False })

-- | (primitive) The layout is stretchable and can be assigned a larger area in both the horizontal and vertical
-- direction. See also combinators like 'fill' and 'floatCentre'.
stretch :: Layout -> Layout
stretch layout
  = updateOptions layout (\options' -> options'{ stretchV = True, stretchH = True })

-- | (primitive) The layout is stretchable in the vertical direction. See also combinators like 'vfill' and 'vfloatCentre'.
vstretch :: Layout -> Layout
vstretch layout
  = updateOptions layout (\options' -> options'{ stretchV = True, stretchH = False })

-- | (primitive) The layout is stretchable in the horizontal direction. See also combinators like 'hfill' and 'hfloatCentre'.
hstretch :: Layout -> Layout
hstretch layout
  = updateOptions layout (\options' -> options'{ stretchH = True, stretchV = False })


-- | Add a margin of a certain width around the entire layout.
margin :: Int -> Layout -> Layout
margin i layout
  = updateOptions layout (\options' -> options'{ margins = [MarginLeft,MarginRight,MarginTop,MarginBottom], marginW = i })

-- | (primitive) Set the width of the margin (default is 10 pixels).
marginWidth :: Int -> Layout -> Layout
marginWidth w layout
  = updateOptions layout (\options' -> options'{ marginW = w })

-- | (primitive) Remove the margin of a layout (default).
marginNone  :: Layout -> Layout
marginNone layout
  = updateOptions layout (\options' -> options'{ margins = [] })

-- | (primitive) Add a margin to the left.
marginLeft  :: Layout -> Layout
marginLeft layout
  = updateOptions layout (\options' -> options'{ margins = MarginLeft:margins options' })

-- | (primitive) Add a right margin.
marginRight  :: Layout -> Layout
marginRight layout
  = updateOptions layout (\options' -> options'{ margins = MarginRight:margins options' })

-- | (primitive) Add a margin to the top.
marginTop  :: Layout -> Layout
marginTop layout
  = updateOptions layout (\options' -> options'{ margins = MarginTop:margins options' })

-- | (primitive) Add a margin to the bottom.
marginBottom  :: Layout -> Layout
marginBottom layout
  = updateOptions layout (\options' -> options'{ margins = MarginBottom:margins options' })


-- | (primitive) Align horizontally to the left when the layout is assigned to a larger area (default).
halignLeft :: Layout -> Layout
halignLeft layout
  = updateOptions layout (\options' -> options'{ alignH = AlignLeft })

-- | (primitive) Align horizontally to the right when the layout is assigned to a larger area.
halignRight :: Layout -> Layout
halignRight layout
  = updateOptions layout (\options' -> options'{ alignH = AlignRight })

-- | (primitive) Center horizontally when assigned to a larger area.
halignCenter :: Layout -> Layout
halignCenter 
  = halignCentre

-- | (primitive) Center horizontally when assigned to a larger area.
halignCentre :: Layout -> Layout
halignCentre layout
  = updateOptions layout (\options' -> options'{ alignH = AlignHCentre })

-- | (primitive) Align vertically to the top when the layout is assigned to a larger area (default).
valignTop :: Layout -> Layout
valignTop layout
  = updateOptions layout (\options' -> options'{ alignV = AlignTop })

-- | (primitive) Align vertically to the bottom when the layout is assigned to a larger area.
valignBottom :: Layout -> Layout
valignBottom layout
  = updateOptions layout (\options' -> options'{ alignV = AlignBottom })

-- | (primitive) Center vertically when the layout is assigned to a larger area.
valignCenter :: Layout -> Layout
valignCenter layout
  = valignCentre layout

-- | (primitive) Center vertically when the layout is assigned to a larger area.
valignCentre :: Layout -> Layout
valignCentre layout
  = updateOptions layout (\options' -> options'{ alignV = AlignVCentre })


-- | Adjust the minimal size of a control dynamically when the content changes.
-- This is used for example to correctly layout static text or buttons when the
-- text or label changes at runtime. This property is automatically set for
-- 'StaticText', 'label's, and 'button's.
dynamic :: Layout -> Layout
dynamic layout
  = updateOptions layout (\options' -> options'{ adjustMinSize = True })

updateOptions :: Layout -> (LayoutOptions -> LayoutOptions) -> Layout
updateOptions layout f
  = layout{ options = f (options layout) }

{-----------------------------------------------------------------------------------------
  primitive layouts
-----------------------------------------------------------------------------------------}
-- | (primitive) Create a labeled border around a layout (= 'StaticBox').
-- Just like a 'grid', the horizontal or vertical stretch of the child layout determines
-- the stretch and expansion mode of the box.
boxed :: String -> Layout -> Layout
boxed txt' content'
  = TextBox optionsDefault{ stretchV = hasvstretch, stretchH = hashstretch
                          , fillMode = hasfill, adjustMinSize = True }
      txt' (extramargin content')
  where
    hasvstretch  = stretchV (options content')
    hashstretch  = stretchH (options content')
    hasfill   = if (hasvstretch || hashstretch) then Fill else FillNone

    extramargin | null (margins (options content')) = marginWidth 5 . marginTop
                | otherwise                        = id

-- | (primitive) Create a static label label (= 'StaticText').
label :: String -> Layout
label txt'
  = Label optionsDefault txt'

-- | (primitive) The expression (@grid w h rows@) creates a grid of @rows@. The @w@ argument
-- is the extra horizontal space between elements and @h@ the extra vertical space between elements.
-- (implemented using the 'FlexGrid' sizer).
--
-- Only when /all/ elements of a column have horizontal stretch (see 'stretch' and 'hstretch'), the entire
-- column will stretch horizontally, and the same holds for rows with vertical stretch.
-- When any column or row in a grid can stretch, the grid itself will also stretch in that direction
-- and the grid will 'expand' to fill the assigned area by default (instead of being 'static').
grid :: Int -> Int -> [[Layout]] -> Layout
grid w h rows'
  = Grid optionsDefault{ stretchV = hasvstretch, stretchH = hashstretch, fillMode = hasfill } (sz w h) rows'
  where
    hasvstretch  = any (all (stretchV.options)) rows'
    hashstretch  = any (all (stretchH.options)) (transpose rows')
    hasfill   = if (hasvstretch || hashstretch) then Fill else FillNone

-- | (primitive) Add a container widget (for example, a 'Panel').
-- Just like a 'grid', the horizontal or vertical stretch of the child layout determines
-- the stretch and expansion mode of the container.
container :: Window a -> Layout -> Layout
container window layout
  = WidgetContainer optionsDefault{ stretchV = hasvstretch, stretchH = hashstretch, fillMode = hasfill }
          (downcastWindow window) layout
  where
    hasvstretch  = stretchV (options layout)
    hashstretch  = stretchH (options layout)
    hasfill   = if (hasvstretch || hashstretch) then Fill else FillNone


-- | (primitive) Lift a basic control to a 'Layout'.
layoutFromWindow :: Window a -> Layout
layoutFromWindow window
  = Widget optionsDefault{ adjustMinSize = adjust } (downcastWindow window)
  where
    adjust  =  instanceOf window classButton 
            || instanceOf window classStaticText


-- | (primitive) Empty layout with a given width and height.
space :: Int -> Int -> Layout
space w h
  = Spacer optionsDefault (Size w h)

-- | (primitive) A line with a given width and height
-- Not all ports (notably not wxGTK) support specifying the transversal
-- direction of the line (e.g. height for a horizontal line)
rule :: Int -> Int -> Layout
rule w h
  = Line optionsDefault (Size w h)


-- | A vertical line with a given height.
vrule :: Int -> Layout
vrule h
  = rule 1 h

-- | A horizontal line with a given width.
hrule :: Int -> Layout
hrule w
  = rule w 1

-- | (primitive) Create a 'Layout' from a 'Sizer' object.
sizer :: Sizer a -> Layout
sizer s
  = XSizer optionsDefault (downcastSizer s)


-- | A tab page in a notebook: a title, a possible bitmap and a layout.
type TabPage  = (String,Bitmap (),Layout)

-- | Create a simple tab page with a certain title and layout.
tab :: String -> Layout -> TabPage
tab title layout
  = (title,objectNull,layout)

-- | Create a tab page with a certain title, icon, and layout.
imageTab :: String -> Bitmap () -> Layout -> TabPage
imageTab title bitmap layout
  = (title,bitmap,layout)

-- | Create a notebook layout.
-- The pages /always/ need to be embedded inside a 'container' (normally a 'Panel'). 
-- Just like a 'grid', the horizontal or vertical stretch of the child layout determines
-- the stretch and expansion mode of the notebook.
tabs :: Notebook a -> [TabPage] -> Layout
tabs notebook pages'
  = XNotebook optionsDefault{ stretchV = hasvstretch, stretchH = hashstretch, fillMode = hasfill }
              (downcastNotebook notebook) pages'
  where
    hasvstretch  = all stretchV [options layout | (_,_,layout) <- pages']
    hashstretch  = all stretchH [options layout | (_,_,layout) <- pages']
    hasfill      = if (hasvstretch || hashstretch) then Fill else FillNone

-- | Add a horizontal sash bar between two windows. The two integer
-- arguments specify the width of the sash bar (5) and the initial
-- height of the top pane respectively.
hsplit :: SplitterWindow a -> Int -> Int -> Layout -> Layout -> Layout
hsplit
  = split True

-- | Add a vertical sash bar between two windows. The two integer
-- arguments specify the width of the sash bar (5) and the initial
-- width of the left pane respectively. 
vsplit :: SplitterWindow a -> Int -> Int -> Layout -> Layout -> Layout
vsplit
  = split False

split :: Bool -> SplitterWindow a -> Int -> Int -> Layout -> Layout -> Layout
split splitHorizontal' splitter' sashWidth' paneWidth' pane1' pane2'
  = Splitter optionsDefault (downcastSplitterWindow splitter') pane1' pane2' splitHorizontal' sashWidth' paneWidth'


optionsDefault :: LayoutOptions
optionsDefault
  = LayoutOptions False False [] 10 AlignLeft AlignTop FillNone Nothing False





{-----------------------------------------------------------------------------------------
  Layout algorithm
-----------------------------------------------------------------------------------------}
-- | Abstract data type that represents the layout of controls in a window.
data Layout = Grid      { options :: LayoutOptions, gap  :: Size, rows :: [[Layout]] }
            | Widget    { options :: LayoutOptions, win  :: Window () }
            | Spacer    { options :: LayoutOptions, spacesize :: Size   }
            | Label     { options :: LayoutOptions, txt  :: String    }
            | TextBox   { options :: LayoutOptions, txt  :: String, content :: Layout }
            | Line      { options :: LayoutOptions, linesize :: Size }
            | XSizer    { options :: LayoutOptions, xsizer :: Sizer () }
            | WidgetContainer{ options :: LayoutOptions, win :: Window (), content :: Layout }
            | XNotebook { options :: LayoutOptions, nbook :: Notebook (), pages :: [(String,Bitmap (),Layout)] }
            | Splitter  { options :: LayoutOptions, splitter :: SplitterWindow ()
                        , pane1 :: Layout, pane2 :: Layout
                        , splitHorizontal :: Bool, sashWidth :: Int, paneWidth :: Int }

data LayoutOptions
           = LayoutOptions{ stretchH :: Bool, stretchV :: Bool
                          , margins :: [Margin], marginW :: Int
                          , alignH :: HAlign, alignV :: VAlign
                          , fillMode :: FillMode
                          , minSize  :: Maybe Size
                          , adjustMinSize :: Bool
                          }

data FillMode = FillNone | FillShaped | Fill
data HAlign   = AlignLeft | AlignRight | AlignHCentre
data VAlign   = AlignTop | AlignBottom | AlignVCentre
data Margin   = MarginTop | MarginLeft | MarginRight | MarginBottom


-- This is just to remove "Defined but not used" warnings:
nullLayoutOptions :: LayoutOptions
nullLayoutOptions = 
  LayoutOptions
    False False
    [] 0
    AlignHCentre AlignVCentre
    FillNone
    Nothing
    False

-- This is just to remove "Defined but not used" warnings:
nullLayout :: Layout
-- Grid      { options :: LayoutOptions, gap  :: Size, rows :: [[Layout]] }
nullLayout = nullLayouts !! 0

-- This is just to remove "Defined but not used" warnings:
nullLayouts :: [Layout]
nullLayouts = 
  [ Grid            { options = nullLayoutOptions, gap = (Size 0 0)
                    , rows = [[]]
                    }
  , Widget          { options = nullLayoutOptions, win = objectNull       }
  , Spacer          { options = nullLayoutOptions, spacesize = (Size 0 0) }
  , Label           { options = nullLayoutOptions, txt = ""               }
  , TextBox         { options = nullLayoutOptions, txt = ""
                    , content = nullLayout
                    }
  , Line            { options = nullLayoutOptions, linesize = (Size 0 0)  }
  , XSizer          { options = nullLayoutOptions, xsizer = objectNull    }
  , WidgetContainer { options = nullLayoutOptions, win = objectNull
                    , content = nullLayout
                    }
  , XNotebook       { options = nullLayoutOptions, nbook = objectNull
                    , pages = [("", objectNull, nullLayout)]
                    }
  , Splitter        { options = nullLayoutOptions, splitter = objectNull
                    , pane1 = nullLayout, pane2 = nullLayout
                    , splitHorizontal = False, sashWidth = 0, paneWidth = 0
                    }
  ]


-- | Fits a widget properly by calling 'windowReLayout' on
-- the parent frame or dialog ('windowGetFrameParent').
windowReFit :: Window a -> IO ()
windowReFit w
  = do p <- windowGetFrameParent w
       windowReLayout p

-- | Fits a widget properly by calling 'windowReLayout' on
-- the parent frame or dialog ('windowGetFrameParent').
windowReFitMinimal :: Window a -> IO ()
windowReFitMinimal w
  = do p <- windowGetFrameParent w
       windowReLayoutMinimal p

-- | Re-invoke layout algorithm to fit a window around its
-- children. It will enlarge when the current
-- client size is too small, but not shrink when the window
-- is already large enough. (in contrast, 'windowReLayoutMinimal' will
-- also shrink a window so that it always minimally sized).
windowReLayout :: Window a -> IO ()
windowReLayout w
  = do _   <- windowLayout w
       old <- windowGetClientSize w
       szr <- windowGetSizer w
       when (not (objectIsNull szr)) (sizerSetSizeHints szr w)
       windowFit w
       new <- windowGetClientSize w
       windowSetClientSize w (sizeMax old new)

-- | Re-invoke layout algorithm to fit a window around its
-- children. It will resize the window to its minimal 
-- acceptable size ('windowFit').
windowReLayoutMinimal :: Window a -> IO ()  
windowReLayoutMinimal w
  = do _   <- windowLayout w
       szr <- windowGetSizer w
       when (not (objectIsNull szr)) (sizerSetSizeHints szr w)
       windowFit w

-- | Set the layout of a window (automatically calls 'sizerFromLayout').
windowSetLayout :: Window a -> Layout -> IO ()
windowSetLayout window layout
  = do sizer' <- sizerFromLayout window layout
       windowSetAutoLayout window True
       windowSetSizer window sizer'
       sizerSetSizeHints sizer' window
       return ()

-- | Create a 'Sizer' from a 'Layout' and a parent window.
sizerFromLayout :: Window a -> Layout -> IO (Sizer ())
sizerFromLayout parent layout
  = insert objectNull (grid 0 0 [[stretch layout]])
  where
    insert :: Sizer () -> Layout -> IO (Sizer ())
    insert container' (Spacer options' sz')
      = do sizerAddWithOptions 0 (sizerAdd container' sz') (\_sz -> return ()) options'
           return container'

    insert container' (Widget options' win')
      = do sizerAddWindowWithOptions container' win' options'
           return container'

    insert container' (Grid goptions gap' rows')
      = do g <- flexGridSizerCreate (length rows') (maximum (map length rows')) (sizeH gap') (sizeW gap')
           mapM_ (stretchRow g) (zip [0..] (map (all (stretchV.options)) rows'))
           mapM_ (stretchCol g) (zip [0..] (map (all (stretchH.options)) (transpose rows')))
           mapM_ (insert (downcastSizer g)) (concat rows')
           when (container' /= objectNull) 
             (sizerAddSizerWithOptions container' g goptions)
           return (downcastSizer g)

    insert container' (Label options' txt')
      = do t <- staticTextCreate parent idAny txt' rectNull 0
           sizerAddWindowWithOptions container' t options'
           return container'

    insert container' (TextBox options' txt' layout')
      = do box    <- staticBoxCreate parent idAny txt' rectNull (wxCLIP_CHILDREN .+. wxNO_FULL_REPAINT_ON_RESIZE)
           sizer' <- staticBoxSizerCreate box wxVERTICAL
           _      <- insert (downcastSizer sizer') layout'
           when (container' /= objectNull) 
             (sizerAddSizerWithOptions container' sizer' options')
           windowLower box
           return (downcastSizer sizer')

    insert container' (Line options' (Size w h))
      = do l <- staticLineCreate parent idAny (rectNull{ rectWidth = w, rectHeight = h }) 
                  (if (w >= h) then wxHORIZONTAL else wxVERTICAL)
           sizerAddWindowWithOptions container' l options'
           return container'

    insert container' (XSizer options' sizer')
      = do sizerAddSizerWithOptions container' sizer' options'
           return container'

    insert container' (WidgetContainer options' win' layout')
      = do windowSetLayout win' layout' -- recursively set the layout' in the window itself
           sizerAddWindowWithOptions container' win' options'
           return container'

    insert container' (Splitter options' splitter' pane1' pane2' splitHorizontal' _sashWidth paneWidth')
      = do splitterWindowSetMinimumPaneSize splitter' 20
           sizerAddWindowWithOptions container' splitter' options'
           _ <- if splitHorizontal'
                  then splitterWindowSplitHorizontally splitter' win1 win2 paneWidth'
                  else splitterWindowSplitVertically   splitter' win1 win2 paneWidth'
           paneSetLayout pane1'
           paneSetLayout pane2'
           
           return container'
      where
        win1  = getWinFromLayout pane1'
        win2  = getWinFromLayout pane2'

        getWinFromLayout layout'
          = case layout' of
              Widget _ win'            -> downcastWindow win'
              WidgetContainer _ win' _ -> downcastWindow win'
              Splitter _ splitter'' _ _ _ _ _ -> downcastWindow splitter''
              _other                   -> error "Layout: hsplit/vsplit need widgets or containers as arguments"

        paneSetLayout layout'
          = case layout' of
              Widget _ _win
                -> return ()
              
              WidgetContainer _options win' layout''
                -> windowSetLayout win' layout''
              
              Splitter _options splitter'' pane1'' pane2'' splitHorizontal'' _sashWidth paneWidth'' 
                ->  do splitterWindowSetMinimumPaneSize splitter'' 20
                       -- sizerAddWindowWithOptions container' splitter'' options'
                       let win1' = getWinFromLayout pane1''
                           win2' = getWinFromLayout pane2''
                       _ <- if splitHorizontal''
                              then splitterWindowSplitHorizontally splitter'' win1' win2' paneWidth''
                              else splitterWindowSplitVertically   splitter'' win1' win2' paneWidth''
                       paneSetLayout pane1''
                       paneSetLayout pane2''
                       return ()
              _other
                -> error "Layout: hsplit/vsplit need widgets or containers as arguments"


    insert container' (XNotebook options' nbook' pages')
      = do pages'' <- addImages objectNull pages'
           mapM_ addPage pages''
           sizerAddWindowWithOptions container' nbook' options'
           return container'
      where
        addPage (title,idx,WidgetContainer _options win' layout')
          = do pagetitle <- if (null title)
                             then windowGetLabel win'
                             else return title
               _ <- notebookAddPage nbook' win' pagetitle False idx
               windowSetLayout win' layout'  -- recursively set layout'

        addPage (_title, _idx, _other)
          = error "Graphics.UI.WXCore.sizerFromLayout: notebook page needs to be a 'container' layout!"

        addImages il []
          = if (objectIsNull il)
             then return []
             else do notebookAssignImageList nbook' il
                     return []

        addImages il ((title, bm, layout'):xs) | objectIsNull bm 
          = do xs' <- addImages il xs
               return ((title,-1,layout'):xs')

        addImages il ((title, bm, layout'):xs)
          = do il' <- addImage il bm
               i   <- imageListGetImageCount il'
               xs' <- addImages il' xs
               return ((title,i,layout'):xs')

        addImage il bm
          = if (objectIsNull il)
             then do w   <- bitmapGetWidth bm
                     h   <- bitmapGetHeight bm
                     il' <- imageListCreate (sz w h) False 1
                     _   <- imageListAddBitmap il' bm objectNull
                     return il'
             else imageListAddBitmap il bm objectNull >>
                  return il


    stretchRow g (i,stretch')
      = when stretch' (flexGridSizerAddGrowableRow g i)

    stretchCol g (i,stretch')
      = when stretch' (flexGridSizerAddGrowableCol g i)

    

    sizerAddWindowWithOptions :: Sizer a -> Window b -> LayoutOptions -> IO ()
    sizerAddWindowWithOptions container' window options'
      = sizerAddWithOptions (flagsAdjustMinSize window options') 
                            (sizerAddWindow container' window) (sizerSetItemMinSizeWindow container' window) options'

    sizerAddSizerWithOptions :: Sizer a -> Sizer b -> LayoutOptions -> IO ()
    sizerAddSizerWithOptions container' sizer' options'
      = sizerAddWithOptions 0 (sizerAddSizer container' sizer') (sizerSetItemMinSizeSizer container' sizer') options'
           

    sizerAddWithOptions :: Int -> (Int -> Int -> Int -> Ptr p -> IO ()) -> (Size -> IO ()) -> LayoutOptions -> IO ()
    sizerAddWithOptions miscflags addSizer setMinSize options'
      = do addSizer 1 (flags options' .+. miscflags) (marginW options') ptrNull
           case minSize options' of
             Nothing -> return ()
             Just sz' -> setMinSize sz'

    flags options'
      = flagsFillMode (fillMode options') .+. flagsMargins (margins options')
        .+. flagsHAlign (alignH options') .+. flagsVAlign (alignV options')

    flagsFillMode fillMode'
      = case fillMode' of
          FillNone    -> 0
          FillShaped  -> wxSHAPED
          Fill        -> wxEXPAND

    flagsHAlign halign
      = case halign of
          AlignLeft    -> wxALIGN_LEFT
          AlignRight   -> wxALIGN_RIGHT
          AlignHCentre -> wxALIGN_CENTRE_HORIZONTAL

    flagsVAlign valign
      = case valign of
          AlignTop     -> wxALIGN_TOP
          AlignBottom  -> wxALIGN_BOTTOM
          AlignVCentre -> wxALIGN_CENTRE_VERTICAL

    flagsMargins margins'
      = bits (map flagsMargin margins')

    flagsMargin margin'
      = case margin' of
          MarginTop    -> wxTOP
          MarginLeft   -> wxLEFT
          MarginBottom -> wxBOTTOM
          MarginRight  -> wxRIGHT
 
--  wxADJUST_MINSIZE is deprecated 
    flagsAdjustMinSize _window _options = 0
{-
      = if (adjustMinSize options) 
         then wxADJUST_MINSIZE
         else 0
-}

{-      
        case minSize options of
          Nothing | -- dleijen: unfortunately, wxADJUST_MINSIZE has bugs for certain controls:
                    not ( instanceOf window classGauge || instanceOf window classGauge95 
                         || instanceOf window classGaugeMSW 
                         || instanceOf window classSlider || instanceOf window classSlider95 
                         || instanceOf window classSliderMSW 
                        )
                  -> wxADJUST_MINSIZE
          other   -> 0
-}
