
{-# LANGUAGE CPP #-}

import qualified Control.Exception as E
import Control.Monad (when, filterM)
import Data.Char (isSpace)
import Data.List (foldl', intersperse, intercalate, nub, lookup, isPrefixOf, isInfixOf, find)
import Data.Maybe (fromJust)
import Distribution.PackageDescription hiding (includeDirs)
import qualified Distribution.PackageDescription as PD (includeDirs)
import Distribution.InstalledPackageInfo(sourcePackageId, includeDirs)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, installedPkgs, withPrograms, buildDir)
import Distribution.Simple.PackageIndex(SearchResult (..), searchByName, allPackages )
import Distribution.Simple.Program (ConfiguredProgram (..), lookupProgram, runProgram, simpleProgram, locationPath)
import Distribution.Simple.Program.Types
import Distribution.Simple.Setup (ConfigFlags, BuildFlags)
import Distribution.System (OS (..), Arch (..), buildOS, buildArch)
import Distribution.Verbosity (normal, verbose)
import System.Process (system)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getModificationTime)
import System.Environment (getEnv)
import System.FilePath ((</>), (<.>), replaceExtension, takeFileName, dropFileName, addExtension, takeDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

wxcoreDirectory  :: FilePath
wxcoreDirectory  = "src" </> "haskell" </> "Graphics" </> "UI" </> "WXCore"

wxcoreDirectoryQuoted  :: FilePath
wxcoreDirectoryQuoted  = "\"" ++ wxcoreDirectory ++ "\""

stripR :: String -> String
stripR = reverse . dropWhile isSpace . reverse

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Comment out type signature because of a Cabal API change from 1.6 to 1.7
myConfHook (pkg0, pbi) flags = do
    createDirectoryIfMissing True wxcoreDirectory
    
#if defined(freebsd_HOST_OS) || defined (netbsd_HOST_OS)
    -- Find GL/glx.h include path using pkg-config
    glIncludeDirs <- readProcess "pkg-config" ["--cflags", "gl"] "" `E.onException` return ""
#else
    let glIncludeDirs = ""
#endif

    lbi <- confHook simpleUserHooks (pkg0, pbi) flags
    wxcIncludeDir <- stripR <$> readProcess "pkg-config" ["--variable=includedir", "wxc"] ""
    let wxcoreIncludeFile  = "\"" ++ wxcIncludeDir </> "wxc.h\""
    let wxcIncludeDirQuoted = "\"" ++ wxcIncludeDir ++ "\""
    let system' command    = putStrLn command >> system command

    putStrLn "Generating class type definitions from .h files"
    system' $ "wxdirect -t --wxc " ++ wxcIncludeDirQuoted ++ " -o " ++ wxcoreDirectoryQuoted ++ " " ++ wxcoreIncludeFile

    putStrLn "Generating class info definitions"
    system' $ "wxdirect -i --wxc " ++ wxcIncludeDirQuoted ++ " -o " ++ wxcoreDirectoryQuoted ++ " " ++ wxcoreIncludeFile

    putStrLn "Generating class method definitions from .h files"
    system' $ "wxdirect -c --wxc " ++ wxcIncludeDirQuoted ++ " -o " ++ wxcoreDirectoryQuoted ++ " " ++ wxcoreIncludeFile

    let lpd       = localPkgDescr lbi
    let lib       = fromJust (library lpd)
    let libbi     = libBuildInfo lib
    let custom_bi = customFieldsBI libbi

    -- TODO ideally we'd let Cabal do this through pkgconfig-depends, and we'd
    -- use Setup.hs only to call wxdirect.
    let libbi' = libbi
          { extraLibs      = extraLibs      libbi ++ ["wxc"]
          , PD.includeDirs = PD.includeDirs libbi ++ [wxcIncludeDir] ++ case glIncludeDirs of
                                                         ('-':'I':v) -> [v];
                                                         _           -> []
          }

    let lib' = lib { libBuildInfo = libbi' }
    let lpd' = lpd { library = Just lib' }

    return $ lbi { localPkgDescr = lpd' }

