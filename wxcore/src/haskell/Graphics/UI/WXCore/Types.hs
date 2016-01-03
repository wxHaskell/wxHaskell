{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK prune #-}
-----------------------------------------------------------------------------------------
{-|
Module      :  Types
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Basic types and operations.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Types(
            -- * Objects
              ( # )
            , Object, objectNull, objectIsNull, objectCast, objectIsManaged
            , objectDelete
            , withObjectPtr, withObjectRef
            , withObjectResult, withManagedObjectResult
            , objectFinalize, objectNoFinalize
            , objectFromPtr, managedObjectFromPtr
            

--            , Managed, managedNull, managedIsNull, managedCast, createManaged, withManaged, managedTouch

            -- * Identifiers
            , Id, idAny, idCreate

            -- * Bits
            , (.+.), (.-.)
            , bits
            , bitsSet

            -- * Control
            , unitIO, bracket, bracket_, finally, finalize, when

            -- * Variables
            , Var, varCreate, varGet, varSet, varUpdate, varSwap

            -- * Misc.
            , Style
            , EventId
            , TreeItem, treeItemInvalid, treeItemIsOk

            -- * Basic types

            -- ** Booleans
            , toCBool, fromCBool

            -- ** Colors
            , Color, rgb, colorRGB, colorRed, colorGreen, colorBlue, intFromColor, colorFromInt, colorIsOk, colorOk
            , black, darkgrey, dimgrey, mediumgrey, grey, lightgrey, white
            , red, green, blue
            , cyan, magenta, yellow
            -- *** System colors
            , SystemColor(..), colorSystem

            -- ** Points
            , Point, Point2(Point,pointX,pointY), point, pt, pointFromVec, pointFromSize, pointZero, pointNull
            , pointMove, pointMoveBySize, pointAdd, pointSub, pointScale

            -- ** Sizes
            , Size, Size2D(Size,sizeW,sizeH), sz, sizeFromPoint, sizeFromVec, sizeZero, sizeNull, sizeEncloses
            , sizeMin, sizeMax

            -- ** Vectors
            , Vector, Vector2(Vector,vecX,vecY), vector, vec, vecFromPoint, vecFromSize, vecZero, vecNull
            , vecNegate, vecOrtogonal, vecAdd, vecSub, vecScale, vecBetween, vecLength
            , vecLengthDouble

            -- ** Rectangles
            , Rect, Rect2D(Rect,rectLeft,rectTop,rectWidth,rectHeight)
            , rectTopLeft, rectTopRight, rectBottomLeft, rectBottomRight, rectBottom, rectRight
            , rect, rectBetween, rectFromSize, rectZero, rectNull, rectSize, rectIsEmpty
            , rectContains, rectMoveTo, rectFromPoint, rectCentralPoint, rectCentralRect, rectStretchTo
            , rectCentralPointDouble, rectCentralRectDouble
            , rectMove, rectOverlaps, rectsDiff, rectUnion, rectOverlap, rectUnions

            ) where

import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses( wxcSystemSettingsGetColour )
import System.IO.Unsafe( unsafePerformIO )

-- utility
import Data.Bits
import Control.Concurrent.STM
import qualified Control.Exception as CE
import qualified Control.Monad as M


infixl 5 .+.
infixl 5 .-.
infix 5 #

-- | Reverse application, i.e. @x # f@ = @f x@.
-- Useful for an object oriented style of programming.
--
-- > (frame # frameSetTitle) "hi"
--
( # ) :: obj -> (obj -> a) -> a
object # method   = method object


{--------------------------------------------------------------------------------
  Bitmasks
--------------------------------------------------------------------------------}
-- | Bitwise /or/ of two bit masks.
(.+.) :: Bits a => a -> a -> a
(.+.) i j
  = i .|. j

-- | Unset certain bits in a bitmask.
(.-.) :: Bits a => a -> a -> a
(.-.) i j
  = i .&. complement j

-- | Bitwise /or/ of a list of bit masks.
bits :: (Num a, Bits a) => [a] -> a
bits xs
  = foldr (.+.) 0 xs

-- | (@bitsSet mask i@) tests if all bits in @mask@ are also set in @i@.
bitsSet :: Bits a => a -> a -> Bool
bitsSet mask i
  = (i .&. mask == mask)


{--------------------------------------------------------------------------------
  Id
--------------------------------------------------------------------------------}
{-# NOINLINE varTopId #-}
varTopId :: Var Id
varTopId
  = unsafePerformIO (varCreate (wxID_HIGHEST+1))

-- | When creating a new window you may specify 'idAny' to let wxWidgets
-- assign an unused identifier to it automatically. Furthermore, it can be
-- used in an event connection to handle events for any identifier.
idAny :: Id
idAny
  = -1

-- | Create a new unique identifier.
idCreate :: IO Id
idCreate
  = varUpdate varTopId (+1)



{--------------------------------------------------------------------------------
  Control
--------------------------------------------------------------------------------}
-- | Ignore the result of an 'IO' action.
unitIO :: IO a -> IO ()
unitIO io
  = io >> return ()

-- | Perform an action when a test succeeds.
when :: Bool -> IO () -> IO ()
when = M.when

-- | Properly release resources, even in the event of an exception.
bracket :: IO a           -- ^ computation to run first (acquire resource)
           -> (a -> IO b) -- ^ computation to run last (release resource)
           -> (a -> IO c) -- ^ computation to run in-between (use resource)
           -> IO c
bracket = CE.bracket

-- | Specialized variant of 'bracket' where the return value is not required.
bracket_ :: IO a     -- ^ computation to run first (acquire resource)
           -> IO b   -- ^ computation to run last (release resource)
           -> IO c   -- ^ computation to run in-between (use resource)
           -> IO c
bracket_ = CE.bracket_

-- | Run some computation afterwards, even if an exception occurs.
finally :: IO a -- ^ computation to run first
        -> IO b -- ^ computation to run last (release resource)
        -> IO a
finally = CE.finally

-- | Run some computation afterwards, even if an exception occurs. Equals 'finally' but
-- with the arguments swapped.
finalize ::  IO b -- ^ computation to run last (release resource)
          -> IO a -- ^ computation to run first
          -> IO a
finalize lastComputation firstComputation
  = finally firstComputation lastComputation

{--------------------------------------------------------------------------------
  Variables
--------------------------------------------------------------------------------}

-- | A mutable variable. Use this instead of 'MVar's or 'IORef's to accommodate for
-- future expansions with possible concurrency.
type Var a  = TVar a

-- | Create a fresh mutable variable.
varCreate :: a -> IO (Var a)
varCreate x    = newTVarIO x

-- | Get the value of a mutable variable.
varGet :: Var a -> IO a
varGet v    = atomically $ readTVar v

-- | Set the value of a mutable variable.
varSet :: Var a -> a -> IO ()
varSet v x = atomically $ writeTVar v x

-- | Swap the value of a mutable variable.
varSwap :: Var a -> a -> IO a
varSwap v x = atomically $ do
                   prev <- readTVar v
                   writeTVar v x
                   return prev

-- | Update the value of a mutable variable and return the old value.
varUpdate :: Var a -> (a -> a) -> IO a
varUpdate v f = atomically $ do
                   x <- readTVar v
                   writeTVar v (f x)
                   return x



{-----------------------------------------------------------------------------------------
  Point
-----------------------------------------------------------------------------------------}
pointMove :: (Num a) => Vector2 a -> Point2 a -> Point2 a
pointMove (Vector dx dy) (Point x y)
  = Point (x+dx) (y+dy)

pointMoveBySize :: (Num a) => Point2 a -> Size2D a -> Point2 a
pointMoveBySize (Point x y) (Size w h)  = Point (x + w) (y + h)

pointAdd :: (Num a) => Point2 a -> Point2 a -> Point2 a
pointAdd (Point x1 y1) (Point x2 y2) = Point (x1+x2) (y1+y2)

pointSub :: (Num a) => Point2 a -> Point2 a -> Point2 a
pointSub (Point x1 y1) (Point x2 y2) = Point (x1-x2) (y1-y2)

pointScale :: (Num a) => Point2 a -> a -> Point2 a
pointScale (Point x y) v = Point (v*x) (v*y)

{- Moved to WxcTypes.hs at 2015-09-01

instance (Num a, Ord a) => Ord (Point2 a) where
  compare (Point x1 y1) (Point x2 y2)             
    = case compare y1 y2 of
        EQ  -> compare x1 x2
        neq -> neq

instance Ix (Point2 Int) where
  range (Point x1 y1,Point x2 y2)             
    = [Point x y | y <- [y1..y2], x <- [x1..x2]]

  inRange (Point x1 y1, Point x2 y2) (Point x y)
    = (x >= x1 && x <= x2 && y >= y1 && y <= y2)

  rangeSize (Point x1 y1, Point x2 y2) 
    = let w = abs (x2 - x1) + 1
          h = abs (y2 - y1) + 1
      in w*h

  index bnd@(Point x1 y1, Point x2 _y2) p@(Point x y)
    = if inRange bnd p
       then let w = abs (x2 - x1) + 1
            in (y-y1)*w + x
       else error ("Point index out of bounds: " ++ show p ++ " not in " ++ show bnd)
-}

{-----------------------------------------------------------------------------------------
  Size
-----------------------------------------------------------------------------------------}
{-
-- | Return the width. (see also 'sizeW').
sizeWidth :: (Num a) => Size2D a -> a
sizeWidth (Size w _h)
  = w

-- | Return the height. (see also 'sizeH').
sizeHeight :: (Num a) => Size2D a -> a
sizeHeight (Size _w h)
  = h
-}

-- | Returns 'True' if the first size totally encloses the second argument.
sizeEncloses :: (Num a, Ord a) => Size2D a -> Size2D a -> Bool
sizeEncloses (Size w0 h0) (Size w1 h1)
  = (w0 >= w1) && (h0 >= h1)

-- | The minimum of two sizes.
sizeMin :: (Num a, Ord a) => Size2D a -> Size2D a -> Size2D a
sizeMin (Size w0 h0) (Size w1 h1)
  = Size (min w0 w1) (min h0 h1)

-- | The maximum of two sizes.
sizeMax :: (Num a, Ord a) => Size2D a -> Size2D a -> Size2D a
sizeMax (Size w0 h0) (Size w1 h1)
  = Size (max w0 w1) (max h0 h1)

{-----------------------------------------------------------------------------------------
  Vector
-----------------------------------------------------------------------------------------}
vecNegate :: (Num a) => Vector2 a -> Vector2 a
vecNegate (Vector x y)
  = Vector (-x) (-y)

vecOrtogonal :: (Num a) => Vector2 a -> Vector2 a
vecOrtogonal (Vector x y) = (Vector y (-x))

vecAdd :: (Num a) => Vector2 a -> Vector2 a -> Vector2 a
vecAdd (Vector x1 y1) (Vector x2 y2) = Vector (x1+x2) (y1+y2)

vecSub :: (Num a) => Vector2 a -> Vector2 a -> Vector2 a
vecSub (Vector x1 y1) (Vector x2 y2) = Vector (x1-x2) (y1-y2)

vecScale :: (Num a) => Vector2 a -> a -> Vector2 a
vecScale (Vector x y) v = Vector (v*x) (v*y)

vecBetween :: (Num a) => Point2 a -> Point2 a -> Vector2 a
vecBetween (Point x1 y1) (Point x2 y2) = Vector (x2-x1) (y2-y1)

vecLength :: Vector -> Double
vecLength (Vector x y)
  = sqrt (fromIntegral (x*x + y*y))

vecLengthDouble :: Vector2 Double -> Double
vecLengthDouble (Vector x y)
  = sqrt (x*x + y*y)

{-----------------------------------------------------------------------------------------
  Rectangle
-----------------------------------------------------------------------------------------}
rectContains :: (Num a, Ord a) => Rect2D a -> Point2 a -> Bool
rectContains (Rect l t w h) (Point x y) 
  = (x >= l && x <= (l+w) && y >= t && y <= (t+h))

rectMoveTo :: (Num a) => Rect2D a -> Point2 a -> Rect2D a
rectMoveTo r p
  = rect p (rectSize r)

rectFromPoint :: (Num a) => Point2 a -> Rect2D a
rectFromPoint (Point x y)
  = Rect x y x y

rectCentralPoint :: Rect2D Int -> Point2 Int
rectCentralPoint (Rect l t w h)
  = Point (l + div w 2) (t + div h 2)

rectCentralRect :: Rect2D Int -> Size -> Rect2D Int
rectCentralRect r@(Rect _l _t _rw _rh) (Size w h)
  = let c = rectCentralPoint r
    in Rect (pointX c - (w - div w 2)) (pointY c - (h - div h 2)) w h

rectCentralPointDouble :: (Fractional a) => Rect2D a -> Point2 a
rectCentralPointDouble (Rect l t w h)
  = Point (l + w/2) (t + h/2)

rectCentralRectDouble :: (Fractional a) => Rect2D a -> Size2D a -> Rect2D a
rectCentralRectDouble r@(Rect _l _t _rw _rh) (Size w h)
  = let c = rectCentralPointDouble r
    in Rect (pointX c - (w - w/2)) (pointY c - (h - h/2)) w h


rectStretchTo :: (Num a) => Rect2D a -> Size2D a -> Rect2D a
rectStretchTo (Rect l t _ _) (Size w h)
  = Rect l t w h

rectMove :: (Num a) => Rect2D a -> Vector2 a -> Rect2D a
rectMove  (Rect x y w h) (Vector dx dy)
  = Rect (x+dx) (y+dy) w h

rectOverlaps :: (Num a, Ord a) => Rect2D a -> Rect2D a -> Bool
rectOverlaps (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2)
  = (x1+w1 >= x2 && x1 <= x2+w2) && (y1+h1 >= y2 && y1 <= y2+h2)


-- | A list with rectangles that constitute the difference between two rectangles.
rectsDiff :: (Num a, Ord a) => Rect2D a -> Rect2D a -> [Rect2D a]
rectsDiff rect1 rect2
  = subtractFittingRect rect1 (rectOverlap rect1 rect2)
  where
    -- subtractFittingRect r1 r2 subtracts r2 from r1 assuming that r2 fits inside r1
    subtractFittingRect :: (Num a, Ord a) => Rect2D a -> Rect2D a -> [Rect2D a]
    subtractFittingRect r1 r2 =
            filter (not . rectIsEmpty)
                    [ rectBetween (rectTopLeft r1) (rectTopRight r2)
                    , rectBetween (pt (rectLeft r1) (rectTop r2)) (rectBottomLeft r2)
                    , rectBetween (pt (rectLeft r1) (rectBottom r2)) (pt (rectRight r2) (rectBottom r1))
                    , rectBetween (rectTopRight r2) (rectBottomRight r1)
                    ]

rectUnion :: (Num a, Ord a) => Rect2D a -> Rect2D a -> Rect2D a
rectUnion r1 r2
  = rectBetween (pt (min (rectLeft r1) (rectLeft r2)) (min (rectTop r1) (rectTop r2)))
         (pt (max (rectRight r1) (rectRight r2)) (max (rectBottom r1) (rectBottom r2)))

rectUnions :: (Num a, Ord a) => [Rect2D a] -> Rect2D a
rectUnions []
  = rectZero
rectUnions (r:rs)
  = foldr rectUnion r rs

-- | The intersection between two rectangles.
rectOverlap :: (Num a, Ord a) => Rect2D a -> Rect2D a -> Rect2D a
rectOverlap r1 r2
  | rectOverlaps r1 r2  = rectBetween (pt (max (rectLeft r1) (rectLeft r2)) (max (rectTop r1) (rectTop r2)))
                               (pt (min (rectRight r1) (rectRight r2)) (min (rectBottom r1) (rectBottom r2)))
  | otherwise           = rectZero


{-----------------------------------------------------------------------------------------
 Default colors.
-----------------------------------------------------------------------------------------}
black, darkgrey, dimgrey, mediumgrey, grey, lightgrey, white :: Color
red, green, blue :: Color
cyan, magenta, yellow :: Color

black     = colorRGB 0x00 0x00 (0x00 :: Int)
darkgrey  = colorRGB 0x2F 0x2F (0x2F :: Int)
dimgrey   = colorRGB 0x54 0x54 (0x54 :: Int)
mediumgrey= colorRGB 0x64 0x64 (0x64 :: Int)
grey      = colorRGB 0x80 0x80 (0x80 :: Int)
lightgrey = colorRGB 0xC0 0xC0 (0xC0 :: Int)
white     = colorRGB 0xFF 0xFF (0xFF :: Int)

red       = colorRGB 0xFF 0x00 (0x00 :: Int)
green     = colorRGB 0x00 0xFF (0x00 :: Int)
blue      = colorRGB 0x00 0x00 (0xFF :: Int)

yellow    = colorRGB 0xFF 0xFF (0x00 :: Int)
magenta   = colorRGB 0xFF 0x00 (0xFF :: Int)
cyan      = colorRGB 0x00 0xFF (0xFF :: Int)


{--------------------------------------------------------------------------
  System colors
--------------------------------------------------------------------------}
-- | System Colors.
data SystemColor
  = ColorScrollBar        -- ^ The scrollbar grey area.  
  | ColorBackground       -- ^ The desktop colour.  
  | ColorActiveCaption    -- ^ Active window caption.  
  | ColorInactiveCaption  -- ^ Inactive window caption.  
  | ColorMenu             -- ^ Menu background.  
  | ColorWindow           -- ^ Window background.  
  | ColorWindowFrame      -- ^ Window frame.  
  | ColorMenuText         -- ^ Menu text.  
  | ColorWindowText       -- ^ Text in windows.  
  | ColorCaptionText      -- ^ Text in caption, size box and scrollbar arrow box.  
  | ColorActiveBorder     -- ^ Active window border.  
  | ColorInactiveBorder   -- ^ Inactive window border.  
  | ColorAppWorkspace     -- ^ Background colour MDI -- ^applications.  
  | ColorHighlight        -- ^ Item(s) selected in a control.  
  | ColorHighlightText    -- ^ Text of item(s) selected in a control.  
  | ColorBtnFace          -- ^ Face shading on push buttons.  
  | ColorBtnShadow        -- ^ Edge shading on push buttons.  
  | ColorGrayText         -- ^ Greyed (disabled) text.  
  | ColorBtnText          -- ^ Text on push buttons.  
  | ColorInactiveCaptionText -- ^ Colour of text in active captions.  
  | ColorBtnHighlight     -- ^ Highlight colour for buttons (same as 3DHILIGHT).  
  | Color3DDkShadow       -- ^ Dark shadow for three-dimensional display elements.  
  | Color3DLight          -- ^ Light colour for three-dimensional display elements.  
  | ColorInfoText         -- ^ Text colour for tooltip controls.  
  | ColorInfoBk           -- ^ Background colour for tooltip controls.  
  | ColorDesktop          -- ^ Same as BACKGROUND.  
  | Color3DFace           -- ^ Same as BTNFACE.  
  | Color3DShadow         -- ^ Same as BTNSHADOW.  
  | Color3DHighlight      -- ^ Same as BTNHIGHLIGHT.  
  | Color3DHilight        -- ^ Same as BTNHIGHLIGHT.  
  | ColorBtnHilight       -- ^ Same as BTNHIGHLIGHT.  

instance Enum SystemColor where
  toEnum _i
    = error "Graphics.UI.WXCore.Types.SytemColor.toEnum: can not convert integers to system colors."

  fromEnum systemColor
    = fromIntegral $ 
       case systemColor of
        ColorScrollBar        -> wxSYS_COLOUR_SCROLLBAR
        ColorBackground       -> wxSYS_COLOUR_BACKGROUND
        ColorActiveCaption    -> wxSYS_COLOUR_ACTIVECAPTION
        ColorInactiveCaption  -> wxSYS_COLOUR_INACTIVECAPTION
        ColorMenu             -> wxSYS_COLOUR_MENU
        ColorWindow           -> wxSYS_COLOUR_WINDOW
        ColorWindowFrame      -> wxSYS_COLOUR_WINDOWFRAME
        ColorMenuText         -> wxSYS_COLOUR_MENUTEXT
        ColorWindowText       -> wxSYS_COLOUR_WINDOWTEXT
        ColorCaptionText      -> wxSYS_COLOUR_CAPTIONTEXT
        ColorActiveBorder     -> wxSYS_COLOUR_ACTIVEBORDER
        ColorInactiveBorder   -> wxSYS_COLOUR_INACTIVEBORDER
        ColorAppWorkspace     -> wxSYS_COLOUR_APPWORKSPACE 
        ColorHighlight        -> wxSYS_COLOUR_HIGHLIGHT
        ColorHighlightText    -> wxSYS_COLOUR_HIGHLIGHTTEXT
        ColorBtnFace          -> wxSYS_COLOUR_BTNFACE
        ColorBtnShadow        -> wxSYS_COLOUR_BTNSHADOW
        ColorGrayText         -> wxSYS_COLOUR_GRAYTEXT
        ColorBtnText          -> wxSYS_COLOUR_BTNTEXT
        ColorInactiveCaptionText -> wxSYS_COLOUR_INACTIVECAPTIONTEXT
        ColorBtnHighlight     -> wxSYS_COLOUR_BTNHIGHLIGHT
        Color3DDkShadow       -> wxSYS_COLOUR_3DDKSHADOW
        Color3DLight          -> wxSYS_COLOUR_3DLIGHT
        ColorInfoText         -> wxSYS_COLOUR_INFOTEXT
        ColorInfoBk           -> wxSYS_COLOUR_INFOBK
        ColorDesktop          -> wxSYS_COLOUR_DESKTOP
        Color3DFace           -> wxSYS_COLOUR_3DFACE
        Color3DShadow         -> wxSYS_COLOUR_3DSHADOW
        Color3DHighlight      -> wxSYS_COLOUR_3DHIGHLIGHT
        Color3DHilight        -> wxSYS_COLOUR_3DHILIGHT
        ColorBtnHilight       -> wxSYS_COLOUR_BTNHILIGHT

      
-- | Convert a system color to a color. 
colorSystem :: SystemColor -> Color
colorSystem systemColor
  = unsafePerformIO $ 
    wxcSystemSettingsGetColour (fromEnum systemColor)
