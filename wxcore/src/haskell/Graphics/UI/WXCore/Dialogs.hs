--------------------------------------------------------------------------------
{-|
Module      :  Dialogs
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Standard dialogs and (non modal) tip windows.
-}
--------------------------------------------------------------------------------
module Graphics.UI.WXCore.Dialogs
    ( 
    -- * Messages
      errorDialog, warningDialog, infoDialog
    , confirmDialog, proceedDialog
    -- ** Non-modal
    , tipWindowMessage, tipWindowMessageBounded

    -- * Files
    , fileOpenDialog
    , filesOpenDialog
    , fileSaveDialog
    , dirOpenDialog
    -- * Misc
    , fontDialog
    , colorDialog
    , passwordDialog
    , textDialog
    , numberDialog
    -- * Internal
    , messageDialog
    , fileDialog
    ) where

import Data.List( intersperse )
import Graphics.UI.WXCore.WxcTypes
import Graphics.UI.WXCore.WxcDefs
import Graphics.UI.WXCore.WxcClasses
import Graphics.UI.WXCore.Types
import Graphics.UI.WXCore.Draw


-- | Opens a non-modal tip window with a text. The window is closed automatically
-- when the user clicks the window or when it loses the focus.
tipWindowMessage :: Window a -> String -> IO ()
tipWindowMessage parent message
  = tipWindowCreate parent message 100 >> return ()

-- | Opens a non-modal tip window with a text. The window is closed automatically
-- when the mouse leaves the specified area, or when the user clicks the window,
-- or when the window loses the focus.
tipWindowMessageBounded :: Window a -> String -> Rect -> IO ()
tipWindowMessageBounded parent message boundingBox
  = do tipWindow <- tipWindowCreate parent message 100
       tipWindowSetBoundingRect tipWindow boundingBox
       return ()

-- | Opens a dialog that lets the user select multiple files. See 'fileOpenDialog' for a description
-- of the arguments. Returns the empty list when the user selected no files or pressed the cancel button.
filesOpenDialog :: Window a -> Bool -> Bool -> String -> [(String,[String])] -> FilePath -> FilePath -> IO [FilePath]
filesOpenDialog parent rememberCurrentDir allowReadOnly message wildcards directory filename
  = fileDialog parent result flags message wildcards directory filename
  where
    flags
      = wxOPEN .+. wxMULTIPLE
         .+. (if rememberCurrentDir then wxCHANGE_DIR else 0)
         .+. (if allowReadOnly then 0 else wxHIDE_READONLY)

    result fd r
      = if (r /= wxID_OK)
         then return []
         else fileDialogGetPaths fd

-- | Show a modal file selection dialog. Usage:
--
-- > fileOpenDialog parent rememberCurrentDir allowReadOnly message wildcards directory filename
--
-- If @rememberCurrentDir@ is 'True', the library changes the current directory to the one where the
-- files were chosen. @allowReadOnly@ determines whether the read-only files can be selected. The @message@
-- is displayed on top of the dialog. The @directory@ is the default directory (use the empty string for
-- the current directory). The @filename@ is the default file name. The @wildcards@ determine the entries
-- in the file selection box. It consists of a list of pairs: the first element is a description (@"Image files"@)
-- and the second element a list of wildcard patterns (@["*.bmp","*.gif"]@).
--
-- > fileOpenDialog frame True True "Open image" [("Any file",["*.*"]),("Bitmaps",["*.bmp"])] "" ""
--
-- Returns 'Nothing' when the user presses the cancel button.
fileOpenDialog  :: Window a -> Bool -> Bool -> String -> [(String,[String])] -> FilePath -> FilePath -> IO (Maybe FilePath)
fileOpenDialog parent rememberCurrentDir allowReadOnly message wildcards directory filename
  = fileDialog parent result flags message wildcards directory filename
  where
    flags
      = wxOPEN .+. (if rememberCurrentDir then wxCHANGE_DIR else 0) .+. (if allowReadOnly then 0 else wxHIDE_READONLY)

    result fd r
      = if (r /= wxID_OK)
         then return Nothing
         else do fname <- fileDialogGetPath fd
                 return (Just fname)

-- | Show a modal file save dialog. Usage:
--
-- > fileSaveDialog parent rememberCurrentDir overwritePrompt message directory filename
--
-- The @overwritePrompt@ argument determines whether the user gets a prompt for confirmation when
-- overwriting a file. The other arguments are as in 'fileOpenDialog'.
fileSaveDialog :: Window a -> Bool -> Bool -> String -> [(String,[String])] -> FilePath -> FilePath -> IO (Maybe FilePath)
fileSaveDialog parent rememberCurrentDir overwritePrompt message wildcards directory filename
  = fileDialog parent result flags message wildcards directory filename
  where
    flags
      = wxSAVE .+. (if rememberCurrentDir then wxCHANGE_DIR else 0) .+. (if overwritePrompt then wxOVERWRITE_PROMPT else 0)

    result fd r
      = if (r /= wxID_OK)
         then return Nothing
         else do fname <- fileDialogGetPath fd
                 return (Just fname)


-- | Generic file dialog function. Takes a function that is called when the dialog is
-- terminated, style flags, a message, a list of wildcards, a directory, and a file name.
-- For example:
-- 
-- > fileOpenDialog  
-- >   = fileDialog parent result flags message wildcards directory filename
-- >   where
-- >     flags
-- >      = wxOPEN .+. (if rememberCurrentDir then wxCHANGE_DIR else 0) 
-- >        .+. (if allowReadOnly then 0 else wxHIDE_READONLY)
-- >
-- >    result fd r
-- >      = if (r /= wxID_OK)
-- >         then return Nothing
-- >         else do fname <- fileDialogGetPath fd
-- >                 return (Just fname)

fileDialog :: Window a -> (FileDialog () -> Style -> IO b) -> Style -> String -> [(String,[String])] -> FilePath -> FilePath -> IO b
fileDialog parent processResult flags message wildcards directory filename
  = bracket
     (fileDialogCreate parent message directory filename (formatWildCards wildcards) pointNull flags)
     (windowDestroy)
     (\fd -> do r <- dialogShowModal fd
                processResult fd r)

  where
    formatWildCards wildcards'
      = concat (intersperse "|"
        [desc ++ "|" ++ concat (intersperse ";" patterns) | (desc,patterns) <- wildcards'])


-- | Show a font selection dialog with a given initial font. Returns 'Nothing' when cancel was pressed.
fontDialog :: Window a -> FontStyle -> IO (Maybe FontStyle)
fontDialog parent fontStyle
  = withFontStyle fontStyle $ \font ->
    bracket (getFontFromUser parent font)
            (fontDelete)
            (\f -> do ok <- fontIsOk f
                      if ok
                       then do info <- fontGetFontStyle f
                               return (Just info)
                       else return Nothing)

{-
  bracket (fontDataCreate) (fontDataDelete) $
      \fdata -> bracket (fontDialogCreate parent fdata) (windowDestroy) $
                   \fd -> do r <- dialogShowModal fd
                             if (r /= wxID_OK)
                               then return Nothing
                               else bracket (fontDataGetChosenFont fdata) (fontDelete) $
                                     \font -> do info <- fontGetFontInfo font
                                                 return (Just info)
-}

-- | Show a color selection dialog given an initial color. Returns 'Nothing' when cancel was pressed.
colorDialog :: Window a -> Color -> IO (Maybe Color)
colorDialog parent color
  = do c <- getColourFromUser parent color
       if (colorIsOk c)
        then return (Just c)
        else return Nothing

-- | Retrieve a password from a user. Returns the empty string on cancel. Usage:
--
-- > passwordDialog window message caption defaultText
--
passwordDialog :: Window a -> String -> String -> String -> IO String
passwordDialog parent message caption defaultText
  = getPasswordFromUser message caption defaultText parent

-- | Retrieve a text string from a user. Returns the empty string on cancel. Usage:
--
-- > textDialog window message caption defaultText
--
textDialog :: Window a -> String -> String -> String -> IO String
textDialog parent message caption defaultText
  = getTextFromUser message caption defaultText parent pointNull False

-- | Retrieve a /positive/ number from a user. Returns 'Nothing' on cancel. Usage:
--
-- > numberDialog window message prompt caption initialValue minimum maximum
--
numberDialog ::  Window a -> String -> String -> String -> Int -> Int -> Int -> IO (Maybe Int)
numberDialog parent message prompt caption value minval maxval
  = let minval' = if minval < 0 then 0 else minval
        maxval' = if maxval < minval' then minval' else maxval
        value'  | value < minval'  = minval'
                | value > maxval'  = maxval'
                | otherwise        = value
    in do i <- getNumberFromUser message prompt caption value' minval' maxval' parent pointNull
          if (i == -1)
           then return Nothing
           else return (Just i)



-- | Show a modal directory dialog. Usage:
--
-- > dirOpenDialog parent allowNewDir message directory
--
-- The @allowNewDir@ argument determines whether the user can create new directories and edit
-- directory names. The @message@ is displayed on top of the dialog and @directory@ is the
-- default directory (or empty for the current directory). Return 'Nothing' when the users
-- presses the cancel button.
dirOpenDialog :: Window a -> Bool -> FilePath -> FilePath -> IO (Maybe FilePath)
dirOpenDialog parent allowNewDir message directory
  = bracket
      (dirDialogCreate parent message directory pointNull flags)
      (windowDestroy)
      (\dd -> do r <- dialogShowModal dd
                 if (r /= wxID_OK)
                  then return Nothing
                  else do path <- dirDialogGetPath dd
                          return (Just path))
  where
    flags
      = if allowNewDir then 0x80 {- wxDD_NEW_DIR_BUTTON -} else 0


-- | An dialog with an /Ok/ and /Cancel/ button. Returns 'True' when /Ok/ is pressed.
--
-- > proceedDialog parent "Error" "Do you want to debug this application?"
--
proceedDialog :: Window a -> String -> String -> IO Bool
proceedDialog parent caption msg
  = do r <- messageDialog parent caption msg (wxOK .+. wxCANCEL .+. wxICON_EXCLAMATION)
       return (r==wxID_OK)

-- | The expression (@confirmDialog caption msg yesDefault parent@) shows a confirm dialog
-- with a /Yes/ and /No/ button. If @yesDefault@ is 'True', the /Yes/ button is default,
-- otherwise the /No/ button. Returns 'True' when the /Yes/ button was pressed.
--
-- > yes <- confirmDialog parent "confirm" "are you sure that you want to reformat the hardisk?"
--
confirmDialog :: Window a -> String -> String -> Bool -> IO Bool
confirmDialog  parent caption msg yesDefault
  = do r <- messageDialog parent caption msg
              (wxYES_NO .+. (if yesDefault then wxYES_DEFAULT else wxNO_DEFAULT) .+. wxICON_QUESTION)
       return (r==wxID_YES)

-- | An warning dialog with a single /Ok/ button.
--
-- > warningDialog parent "warning" "you need a break"
--
warningDialog :: Window a -> String -> String -> IO ()
warningDialog  parent caption msg
  = unitIO (messageDialog parent caption msg (wxOK .+. wxICON_EXCLAMATION))

-- | An error dialog with a single /Ok/ button.
--
-- > errorDialog parent "error" "fatal error, please re-install windows"
--
errorDialog :: Window a -> String -> String -> IO ()
errorDialog parent caption msg
  = unitIO (messageDialog parent caption msg (wxOK .+. wxICON_HAND))

-- | An information dialog with a single /Ok/ button.
--
-- > infoDialog parent "info" "you've got mail"
--
infoDialog :: Window a -> String -> String -> IO ()
infoDialog parent caption msg
  = unitIO (messageDialog parent caption msg (wxOK .+. wxICON_INFORMATION))

-- | A primitive message dialog, specify icons and buttons.
--
-- > r <- messageDialog w "Confirm" "Do you really want that?"
-- >                       (wxYES_NO .+. wxNO_DEFAULT .+. wxICON_QUESTION)
--
messageDialog :: Window a -> String -> String -> BitFlag -> IO BitFlag
messageDialog parent caption msg flags
  = do m <- messageDialogCreate parent msg caption flags
       r <- messageDialogShowModal m
       messageDialogDelete m
       return r
