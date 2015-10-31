{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------------------
{-|
Module      :  Controls
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable
-}
--------------------------------------------------------------------------------
module Graphics.UI.WXCore.Controls
    ( 
      -- * Log
      textCtrlMakeLogActiveTarget
    , logDeleteAndSetActiveTarget
      -- * Tree control
    , TreeCookie
    , treeCtrlGetChildCookie, treeCtrlGetNextChild2
    , treeCtrlWithChildren, treeCtrlGetChildren
    , treeCtrlGetSelections2
      -- * Wrappers
    , listBoxGetSelectionList
    , execClipBoardData
      -- * Font Enumerator
    , enumerateFontsList
    , enumerateFonts
      -- * Deprecated
    , wxcAppUSleep
    ) where

import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types

import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.String(peekCWString)

-- | Get the selections of a tree control.
treeCtrlGetSelections2 :: TreeCtrl a -> IO [TreeItem]
treeCtrlGetSelections2 treeCtrl
  = do xs <- treeCtrlGetSelections treeCtrl
       return (map treeItemFromInt xs)

-- | Represents the children of a tree control.
data TreeCookie   = TreeCookie (Var Cookie)
data Cookie       = Cookie TreeItem
                  | CookieFirst TreeItem
                  | CookieInvalid

-- | Get a @TreeCookie@ to iterate through the children of tree node.
treeCtrlGetChildCookie :: TreeCtrl a -> TreeItem -> IO TreeCookie
treeCtrlGetChildCookie _treeCtrl parent
  = do pcookie <- varCreate (CookieFirst parent)
       return (TreeCookie pcookie)

-- | Get the next child of a tree node. Returns 'Nothing' when
-- the end of the list is reached. This also invalidates the tree cookie.
treeCtrlGetNextChild2 :: TreeCtrl a -> TreeCookie -> IO (Maybe TreeItem)
treeCtrlGetNextChild2 treeCtrl (TreeCookie pcookie)
  = do cookie <- varGet pcookie
       case cookie of
         CookieInvalid    -> return Nothing
         CookieFirst item -> with 0 $ \pint ->
                             do first <- treeCtrlGetFirstChild treeCtrl item pint
                                if (treeItemIsOk first)
                                 then do varSet pcookie (Cookie first)
                                         return (Just first)
                                 else do varSet pcookie (CookieInvalid)
                                         return Nothing
         Cookie item ->
                             do next <- treeCtrlGetNextSibling treeCtrl item
                                if (treeItemIsOk next)
                                 then do varSet pcookie (Cookie next)
                                         return (Just next)
                                 else do varSet pcookie (CookieInvalid)
                                         return Nothing

-- | Iterate on the list of children of a tree node.
treeCtrlWithChildren :: TreeCtrl a -> TreeItem -> (TreeItem -> IO b) -> IO [b]
treeCtrlWithChildren treeCtrl parent f
  = do cookie <- treeCtrlGetChildCookie treeCtrl parent
       let walk acc  = do mbitem <- treeCtrlGetNextChild2 treeCtrl cookie
                          case mbitem of
                            Nothing   -> return (reverse acc)
                            Just item -> do x <- f item
                                            walk (x:acc)
       walk []

-- | Get the children of tree node.
treeCtrlGetChildren :: TreeCtrl a -> TreeItem -> IO [TreeItem]
treeCtrlGetChildren treeCtrl item
  = treeCtrlWithChildren treeCtrl item return

-- | 

-- | Return the current selection in a listbox.
listBoxGetSelectionList :: ListBox a -> IO [Int]
listBoxGetSelectionList listBox
  = do n <- listBoxGetSelections listBox ptrNull 0
       let count = abs n
       allocaArray count $ \carr ->
        do _  <- listBoxGetSelections listBox carr count
           xs <- peekArray count carr
           return (map fromCInt xs)

-- | Sets the active log target and deletes the old one.
logDeleteAndSetActiveTarget :: Log a -> IO ()
logDeleteAndSetActiveTarget log'
  = do oldlog <- logSetActiveTarget log'
       when (not (objectIsNull oldlog)) (logDelete oldlog)
       

-- | Set a text control as a log target.
textCtrlMakeLogActiveTarget :: TextCtrl a -> IO ()
textCtrlMakeLogActiveTarget textCtrl
  = do log' <- logTextCtrlCreate textCtrl
       logDeleteAndSetActiveTarget log'


-- | Use a 'clipboardSetData' or 'clipboardGetData' in this function. But don't
-- use long computation in this function. Because this function encloses the
-- computation with 'clipboardOpen' and 'clipboardClose', and wxHaskell uses
-- Global clipboard on your environment. So, long computation causes problem.
execClipBoardData :: Clipboard a -> (Clipboard a -> IO b) -> IO b
execClipBoardData cl event = bracket_ (clipboardOpen cl) (clipboardClose cl) (event cl)

{-# DEPRECATED wxcAppUSleep "Use wxcAppMilliSleep instead" #-}
-- | This function just left for backward-compatiblity.
-- Update your code to use 'wxcAppMilliSleep' instead.
wxcAppUSleep :: Int -> IO ()
wxcAppUSleep = wxcAppMilliSleep


-- | (@enumerateFontsList encoding fixedWidthOnly@) return the Names of the available fonts in a list. 
-- To get all available fonts call @enumerateFontsList wxFONTENCODING_SYSTEM False@.
-- See also @enumerateFonts@.
enumerateFontsList :: Int -> Bool -> IO [String]
enumerateFontsList encoding fixedWidthOnly = do
  v <- varCreate []
  enumerateFonts encoding fixedWidthOnly $ listFkt v
  varGet v
  where
  listFkt :: Var [String] -> String -> IO Bool
  listFkt v txt = do
    _ <- varUpdate v (txt:)
    return True

foreign import ccall "wrapper" wrapEnumeratorFunc :: (Ptr () -> Ptr CWchar -> IO CInt) -> IO (FunPtr (Ptr () -> Ptr CWchar -> IO CInt))

-- | (@enumerateFonts encoding fixedWidthOnly f@ calls successive @f name@ for the fonts installed on the system.
-- It stops if the function return False.
-- See also @enumerateFontsList@.
enumerateFonts :: Int -> Bool -> (String -> IO Bool) -> IO ()
enumerateFonts encoding fixedWidthOnly fkt = do
  fontEnumerator <- fontEnumeratorCreate ptrNull =<< fuc fkt
  _ <- fontEnumeratorEnumerateFacenames fontEnumerator encoding (fromEnum fixedWidthOnly)
  fontEnumeratorDelete fontEnumerator
  where
    fuc :: (String -> IO Bool) -> IO (Ptr (Ptr () -> Ptr CWchar -> IO CInt))
    fuc f = fmap toCFunPtr $ wrapEnumeratorFunc $ fucH f
    fucH :: (String -> IO Bool) -> Ptr () -> Ptr CWchar -> IO CInt
    fucH f _ cwPtr = do
      continue <- f =<< peekCWString cwPtr
      return $ toCInt $ fromEnum $ continue


