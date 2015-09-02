-----------------------------------------------------------------------------------------
{-|
Module      :  Defines
Copyright   :  (c) Daan Leijen 2003
License     :  wxWindows

Maintainer  :  wxhaskell-devel@lists.sourceforge.net
Stability   :  provisional
Portability :  portable

Exports standard /defines/ of wxWidgets.
-}
-----------------------------------------------------------------------------------------
module Graphics.UI.WXCore.Defines(
            -- * GUI toolkit
            WxToolkit(..), wxToolkit, wxVersion
            -- * Files
            , getAbsoluteFilePath
            , dirSep, pathSep
            ) where

import System.Directory
import System.FilePath
import Graphics.UI.WXCore.WxcClasses
import System.IO.Unsafe( unsafePerformIO )

{--------------------------------------------------------------------------------
  
--------------------------------------------------------------------------------}

-- | wxWidgets library kinds.
data WxToolkit  = WxGTK         -- ^ GTK
                | WxMac         -- ^ MacOS
                | WxMSW         -- ^ Any windows
                | WxMotif       
                | WxMGL         -- ^ SciTech soft MGL
                | WxUniversal
                | WxOSTwo       -- ^ OS\/2
                | WxXEleven     -- ^ X11
                | WxUnknown
                deriving (Eq,Show,Enum)

{-# NOINLINE wxToolkit #-}
-- | Get the current wxWidgets library kind.
wxToolkit :: WxToolkit
wxToolkit
  = unsafePerformIO $ findDefine WxUnknown toolkits
  where
    toolkits  = [("__WXGTK__", WxGTK)
                ,("__WXMAC__", WxMac)
                ,("__WXMSW__", WxMSW)
                ,("__WXMOTIF__", WxMotif)
                ,("__WXMGL__", WxMGL)
                ,("__WXUNIVERSAL__", WxUniversal)
                ,("__WXOS2__", WxOSTwo)
                ,("__WXX11__", WxXEleven)
                ]


findDefine :: a -> [(String,a)] -> IO a
findDefine def []
  = return def
findDefine def ((name,val):xs)
  = do defined <- isDefined name
       if (defined)
        then return val
        else findDefine def xs


{-# NOINLINE wxVersion #-}
-- | Return the version of the wxWidgets library. It is composed of the major
-- version times 1000, plus the minor version times 100, plus the release number.
-- For example, version 2.1.15 would be 2115.
wxVersion :: Int
wxVersion
  = unsafePerformIO $ versionNumber



{-----------------------------------------------------------------------------------------
  relative files
-----------------------------------------------------------------------------------------}

-- | Find a file relative to the application or current directory.
-- (absolute paths are passed without modification). This allows one
-- to access resources relative to the installation directory in a
-- portable way.
getAbsoluteFilePath :: FilePath -> IO FilePath
getAbsoluteFilePath fname
  = do exist <- doesFileExist fname
       if exist
        then return fname
        else do appdir <- getApplicationDir
                let appdirfname = appdir </> fname
                appdirfnameExists  <- doesFileExist appdirfname
                if appdirfnameExists
                 then return appdirfname
                 else do cwd <- getCurrentDirectory 
                         let cwdfname = cwd </> fname
                         cwdfnameExists <- doesFileExist cwdfname
                         if cwdfnameExists
                          then return cwdfname
                          else return fname

{-# DEPRECATED dirSep "Use System.FilePath module and/or its module's pathSeparator instead" #-}
-- | deprecated: Use System.FilePath module and/or its module\'s 'pathSeparator' instead.
dirSep :: String
dirSep
  = case wxToolkit of
      WxMSW   -> "\\"
      _other  -> "/"

{-# DEPRECATED pathSep "Use System.FilePath module's searchPathSeparator instead" #-}
-- | deprecated: Use System.FilePath module\'s 'searchPathSeparator' instead.
pathSep :: String
pathSep
  = case wxToolkit of
      WxMSW   -> ";"
      _other  -> ":"

