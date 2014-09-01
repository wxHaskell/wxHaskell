import Control.Monad (when, filterM)
import Data.List (foldl', intersperse, intercalate, nub, lookup, isPrefixOf, isInfixOf, find)
import Data.Maybe (fromJust)
import Distribution.PackageDescription hiding (includeDirs)
import Distribution.InstalledPackageInfo(installedPackageId, sourcePackageId, includeDirs)
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


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- |This slightly dubious function obtains the install path for the wxc package we are using.
-- It works by finding the wxc package's installation info, then finding the include directory 
-- which contains wxc's headers (amongst the wxWidgets include dirs) and then going up a level.
-- It would be nice if the path was part of InstalledPackageInfo, but it isn't.
wxcInstallDir :: LocalBuildInfo -> IO FilePath
wxcInstallDir lbi = 
    case searchByName (installedPkgs lbi) "wxc" of
        Unambiguous (wxc_pkg:_) -> do
            wxc <- filterM (doesFileExist . (</> "wxc.h")) (includeDirs wxc_pkg)
            case wxc of
                [wxcIncludeDir] -> return (takeDirectory wxcIncludeDir)
                [] -> error "wxcInstallDir: couldn't find wxc include dir"
                _  -> error "wxcInstallDir: I'm confused. I see more than one wxc include directory from the same package"
        Unambiguous [] -> error "wxcInstallDir: Cabal says wxc is installed but gives no package info for it"
        _ -> error "wxcInstallDir: Couldn't find wxc package in installed packages"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Comment out type signature because of a Cabal API change from 1.6 to 1.7
myConfHook (pkg0, pbi) flags = do
    createDirectoryIfMissing True wxcoreDirectory

    lbi <- confHook simpleUserHooks (pkg0, pbi) flags
    wxcDirectory <- wxcInstallDir lbi
    let wxcoreIncludeFile  = "\"" ++ wxcDirectory </> "include" </> "wxc.h\""
    let wxcDirectoryQuoted = "\"" ++ wxcDirectory ++ "\""
    let system' command    = putStrLn command >> system command

    putStrLn "Generating class type definitions from .h files"
    system' $ "wxdirect -t --wxc " ++ wxcDirectoryQuoted ++ " -o " ++ wxcoreDirectoryQuoted ++ " " ++ wxcoreIncludeFile

    putStrLn "Generating class info definitions"
    system' $ "wxdirect -i --wxc " ++ wxcDirectoryQuoted ++ " -o " ++ wxcoreDirectoryQuoted ++ " " ++ wxcoreIncludeFile

    putStrLn "Generating class method definitions from .h files"
    system' $ "wxdirect -c --wxc " ++ wxcDirectoryQuoted ++ " -o " ++ wxcoreDirectoryQuoted ++ " " ++ wxcoreIncludeFile

    let lpd       = localPkgDescr lbi
    let lib       = fromJust (library lpd)
    let libbi     = libBuildInfo lib
    let custom_bi = customFieldsBI libbi

    let libbi' = libbi
          { extraLibDirs = extraLibDirs libbi ++ [wxcDirectory]
          , extraLibs    = extraLibs    libbi ++ ["wxc"]
          , ldOptions    = ldOptions    libbi ++ ["-Wl,-rpath," ++ wxcDirectory]  }

    let lib' = lib { libBuildInfo = libbi' }
    let lpd' = lpd { library = Just lib' }

    return $ lbi { localPkgDescr = lpd' }

