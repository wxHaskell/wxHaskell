-----------------------------------------------------------------------------------------
{-| Module      :  IOExtra
    Copyright   :  (c) Dave Tapley 2011
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net

    Defines most of the classes in wxWindows.
-}
-----------------------------------------------------------------------------------------
module IOExtra( writeFileLazy ) where

import Control.Exception
import Control.Monad
import System.IO.Error
import qualified System.IO.Strict as Strictly

{-----------------------------------------------------------------------------------------
Only write to the file if its contents have changed,
e.g. don't change the file's modifed time unless you change the contents.
-----------------------------------------------------------------------------------------}
writeFileLazy :: FilePath -> String -> IO ()
writeFileLazy f str = fileChanged >>= flip when (writeFile f str)
    where fileChanged = return . either (const True) (/= str)  =<< tryJust (guard . isDoesNotExistError) (Strictly.readFile f)
