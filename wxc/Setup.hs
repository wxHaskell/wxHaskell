
{-# LANGUAGE CPP #-}

import qualified Control.Exception as E
import Control.Monad (filterM, join, liftM2, mapM_, unless, when)
import qualified Data.ByteString.Lazy as B
import Data.Char     ( ord )
import Data.Functor  ( (<$>) )
import Data.List (foldl', foldr, intersperse, intercalate, nub, lookup, isPrefixOf, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing, isJust, listToMaybe)
import Distribution.Compat.Exception (catchIO)
import Distribution.PackageDescription
import Distribution.Pretty(prettyShow)
import Distribution.Simple
import Distribution.Simple.InstallDirs (InstallDirs(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, installedPkgs, withPrograms, buildDir, absoluteInstallDirs)
import Distribution.Simple.PackageIndex(SearchResult (..), searchByName )
import Distribution.Simple.Program (ConfiguredProgram (..), lookupProgram, runProgram, simpleProgram, locationPath)
import Distribution.Simple.Setup ( BuildFlags, ConfigFlags
                                 , CopyDest(..), CopyFlags, copyVerbosity
                                 , InstallFlags, installVerbosity
                                 , fromFlag, fromFlagOrDefault, copyDest
                                 )
import Distribution.Simple.Utils ( die
                                 , installOrdinaryFile
                                 , IOData(..)
                                 , IODataMode(..)
                                 , rawSystemExitWithEnv
                                 , rawSystemStdInOut
                                 )
import Distribution.System (OS (..), Arch (..), buildOS, buildArch)
import Distribution.Verbosity (Verbosity, normal, verbose)
import Distribution.Version(Version(..))
import System.Directory ( createDirectoryIfMissing, doesFileExist
                        , findExecutable,           getCurrentDirectory
                        , getDirectoryContents,     getModificationTime
                        )
import System.Environment (lookupEnv, getEnvironment)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>), (<.>), replaceExtension, takeFileName, dropFileName, addExtension)
import System.IO (hPutStrLn, readFile, stderr)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Process
import System.Process (system, readProcess)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Some utility functions

whenM :: Monad m => m Bool -> m () -> m ()
whenM mp e = mp >>= \p -> when p e


-- Find the first element in a list that matches a condition
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _  []       = return Nothing
findM mp (x : xs) =
  do
    r <- mp x
    if r
      then return $ Just x
      else findM mp xs


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
     { confHook  = myConfHook
     , buildHook = myBuildHook
     , copyHook  = myCopyHook
     , instHook  = myInstHook
     }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

sourceDirectory  :: FilePath
includeDirectory :: FilePath

sourceDirectory  = "cpp"
includeDirectory = "include"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

rawShellSystemStdInOut :: Verbosity                     -- Verbosity level
                       -> FilePath                      -- Path to command
                       -> [String]                      -- Command arguments
                       -> IO (String, String, ExitCode) -- (Command result, Errors, Command exit status)
rawShellSystemStdInOut v f as =
    rawSystemStdInOut v "sh" (f:as) Nothing Nothing Nothing IODataModeText >>=
    \(IODataText result, errors, exitStatus) -> return (result, errors, exitStatus)

isWindowsMsys :: IO Bool
isWindowsMsys = (buildOS == Windows&&) . isJust <$> lookupEnv "MSYSTEM"

-- Comment out type signature because of a Cabal API change from 1.6 to 1.7
myConfHook (pkg0, pbi) flags = do
    let whenExecutableNotFound = whenM (isNothing <$> findExecutable "wx-config")
    mswMsys <- isWindowsMsys
    if mswMsys then do
        (r, e, c) <- rawShellSystemStdInOut normal "wx-config" ["--release"]
        unless (c == ExitSuccess) $
            -- In case you use Stack on Windows which internally use MSYS.
            -- From Windows Console rawShellSystemStdInOut doesn't work
            whenExecutableNotFound $ do
              putStrLn ("Error: MSYS environment wx-config script not found, please install wx-config before installing wxc" ++ "\n"
                        ++ e ++ "\n"
                        ++ show c)
              exitFailure
    else
        whenExecutableNotFound $
        do
            putStrLn "Error: wx-config not found, please install wx-config before installing wxc"
            exitFailure

    whenM bitnessMismatch
      exitFailure

    wxVersion <- checkWxVersion
    generateHeaders wxVersion

    lbi <- confHook simpleUserHooks (pkg0, pbi) flags
    let lpd       = localPkgDescr lbi
    let lib       = fromJust (library lpd)
    let libbi     = libBuildInfo lib
    let custom_bi = customFieldsBI libbi

    wx <- fmap parseWxConfig (readWxConfig wxVersion) >>= deMsysPaths

    let libbi' = libbi
          { extraLibDirs = extraLibDirs libbi ++ extraLibDirs wx
          -- Remove wx libraries from here on windows because archive names differ from dlls
          -- causing GHCI to fail to load them
          , extraLibs    = if buildOS == Windows then
                                extraLibs libbi
                           else
                                extraLibs libbi ++ reverse (extraLibs    wx)
          , ldOptions    = ldOptions    libbi ++ ldOptions    wx
          , frameworks   = frameworks   libbi ++ frameworks   wx
          , includeDirs  = includeDirs  libbi ++ includeDirs  wx
          , ccOptions    = ccOptions    libbi ++ ccOptions    wx ++ ["-DwxcREFUSE_MEDIACTRL"]
          }

    let lib' = lib { libBuildInfo = libbi'    }
    let lpd' = lpd { library      = Just lib' }

    return $ lbi { localPkgDescr = lpd' }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

generateHeaders :: String -> IO ()
generateHeaders wxVersion =
  do
    writeFile ("src" </> "include" </> "wxc_def.h")
      $ cppDefinition "wxVERSION_NUMBER" (wxVERSION_NUMBER wxVersion)

cppDefinition :: Show a => String -> a -> String
cppDefinition name value =
  "#define " ++ name ++ " " ++ show value ++ "\n"

wxVERSION_NUMBER :: String -> Int
wxVERSION_NUMBER wxVersion =
  let (major : minor : remaining) = splitOn "." wxVersion in
    read major * 1000 + read minor * 100 +
      case remaining of
        [] -> 0
        [revision] -> read revision

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

{-
   Bitness check
-}

data Bitness
  = Bits32
  | Bits64
  | Universal
  | Unknown
  deriving Eq


instance Show Bitness where
  show Bits32    = "32"
  show Bits64    = "64"
  show Universal = "Universal"
  show Unknown   = "Unknown"


data CheckResult
  = OK
  | NotOK Bitness Bitness
  | NotChecked
  deriving Eq

{-
   Extract bitness info from a dynamic library and compare to the
   bitness of this program. Works for architectures I386 and X86_64.
-}
checkBitness :: FilePath -> IO CheckResult
checkBitness file =
  if thisBitness == Unknown
    then return NotChecked
    else
      if buildOS == Windows
        then compareBitness <$> getWindowsBitness file
        else compareBitness . readBitness <$> readProcess "file" [file] ""
  where
    getWindowsBitness :: FilePath -> IO Bitness
    getWindowsBitness fp =
      do
        contents <- B.unpack <$> B.readFile file
        if take 2 contents /= [0x4D, 0x5A]   -- "MZ"
          then return Unknown  -- The file is not an executable
          else
            do
              -- The offset of the PE header is at 0x3C.
              -- In the PE header, after "PE\0\0", one finds the type of
              -- machine the executable is compiled for.
              -- According to
              --   http://www.opensource.apple.com/source/cctools/cctools-795/include/coff/ms_dos_stub.h?txt
              -- the index is four byte long. It is in little endian order.
              --
              -- N.B. Might need an update when Windows runs on ARM
              let machineOffsetList = reverse $ take 4 $ drop 0x3C $ contents
              let machineOffset = listToInt machineOffsetList + 4
              return $
                case contents !! machineOffset of
                  0x4C -> Bits32   -- "The file is 32 bit"
                  0x64 -> Bits64   -- "The file is 64 bit"
                  _    -> Unknown  -- "The bitness is not recognized"
        where
          listToInt :: Integral a => [a] -> Int
          listToInt xs = foldl1 (\x y -> 256 * x + y) (map fromIntegral xs)


    compareBitness :: Bitness -> CheckResult
    compareBitness thatBitness =
      if thatBitness == Unknown
        then NotChecked
        else
          if thisBitness == thatBitness ||
             thatBitness == Universal
            then OK
            else NotOK thisBitness thatBitness

    thisBitness =
      case buildArch of
        I386   -> Bits32
        X86_64 -> Bits64
        _      -> Unknown

    readBitness :: String -> Bitness
    readBitness string
      | anyInString [ " i386",   " 80386"
                    , " 32-bit", "AMD386"  ] = Bits32
      | anyInString [ " x86_64", " 64-bit" ] = Bits64
      | anyInString [ "universal binary"   ] = Universal
      | otherwise                            = Unknown
      where
        anyInString :: [String] -> Bool
        anyInString strings = any (`isInfixOf` string) strings

{-
   Return True if this program is 32 bit and the wxWidgets dynamic
   libraries are 64 bits or vice versa. Also, print a result message.

   If there is insufficient data, or the OS is not handled, return
   False, to prevent unnecessary abortion of the install procedure
   N.B. If the installation procedure is simplified, we cannot
   use the file-command on Windows anymore, as it is part of MSYS
   N.B. This does not work if we are cross-compiling
-}
bitnessMismatch :: IO Bool
bitnessMismatch =
  if buildOS `elem` [Windows, Linux, OSX]
    then check
    else return False -- Other OSes are not checked
  where
    check =
      do
        maybeWxwin <- lookupEnv "WXWIN"
        maybeWxcfg <- lookupEnv "WXCFG"
        if isNothing maybeWxwin || isNothing maybeWxcfg
          then return False -- Insufficient data, just continue installing
          else check2 (fromJust maybeWxwin) (fromJust maybeWxcfg)

    check2 wxwin wxcfg =
      do
        let path = normalisePath $ wxwin </> "lib" </> wxcfg </> ".."
        maybeDynamicLibraryName <- getDynamicLibraryName path
        case maybeDynamicLibraryName of
          Nothing                 ->
            putStrLn "Could not find a dynamic library to check bitness, continuing installation" >>
            return False
          Just dynamicLibraryName ->
            check3 path dynamicLibraryName

    check3 path dynamicLibraryName =
      do
        bitnessCheckResult <- checkBitness $ path </> dynamicLibraryName
        case bitnessCheckResult of
          NotOK thisBitness thatBitness ->
            do
              putStrLn $ "Error: The bitness does not match,"
                         ++ " wxHaskell is being compiled as "
                         ++ show thisBitness   ++ " bit, the file "
                         ++ dynamicLibraryName ++ " is "
                         ++ show thatBitness   ++ " bit."
              return True
          OK         ->
            do
              putStrLn $ "The bitness is correct"
              return False
          NotChecked ->
            do
              putStrLn $ "The bitness is not checked"
              return False

    getDynamicLibraryName :: FilePath -> IO (Maybe String)
    getDynamicLibraryName path =
      listToMaybe . filter isLibrary <$> getDirectoryContents path
        `E.onException` return Nothing
        where
          isLibrary x = any (`isPrefixOf` x) ["libwx_base", "wxbase"] &&
                        any (`isInfixOf`  x) [".dll", ".dylib", ".so."]


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- A list of wxWidgets versions that can be handled by this version of wxHaskell
wxCompatibleVersions = ["3.0", "2.9"] -- Preferred version first

checkWxVersion :: IO String
checkWxVersion =
  do
    maybeWxVersion <- findWxVersion

    case maybeWxVersion of
      Nothing ->
        error ("This version of wxc requires one of the following wxWidgets versions to be available: "
               ++ show wxCompatibleVersions
              )
      Just wxVersion ->
        return wxVersion

-- Get the preprocessor/compiler/linker options used for the most recent
-- compatible wxWidgets installed.
-- Abort the setup procedure when no proper version of wxWidgets is found
readWxConfig :: String -> IO String
readWxConfig wxVersion =
  do
    putStrLn ("Configuring wxc to build against wxWidgets " ++ wxVersion)

#if defined(freebsd_HOST_OS) || defined (netbsd_HOST_OS)
    putStrLn "defined(freebsd_HOST_OS) || defined (netbsd_HOST_OS)"
    -- find GL/glx.h on non-Linux systems
    let glIncludeDirs = readProcess "pkg-config" ["--cflags", "gl"] "" `E.onException` return ""
#else
    let glIncludeDirs = return ""
#endif

    -- Additional libraries that are needed for wxc
    -- Note: "all" option doesn't bring them on the popular wx-config-win
    -- (https://github.com/wxHaskell/wxHaskell/tree/master/wx-config-win,
    -- other versions can be found at https://github.com/kowey/wx-config-win
    -- and branches of it)
    let neededLibs = [intercalate "," ["richtext", "xrc", "qa", "html", "adv", "core", "xml", "net", "base", "aui", "propgrid", "ribbon", "gl", "stc"]]

    -- The Windows port of wx-config doesn't let you specify a version (yet)
    isMsys <- isWindowsMsys
    case (buildOS, isMsys) of
      -- wx-config-win does not list all libraries if --cppflags comes after --libs :-(
      (Windows, False) -> liftM2 (++) glIncludeDirs (wx_config $ ["--cppflags", "--libs"] ++ neededLibs)
      (Windows, True)  -> wx_config (["--gl-libs", "--cppflags", "--libs"] ++ neededLibs)
      _                -> liftM2 (++) glIncludeDirs (wx_config ["--version=" ++ wxVersion, "--libs", "all", "--cppflags"])


wx_config :: [String] -> IO String
wx_config parms = do
  let runExecutable failureAction =
        readProcess "wx-config" parms "" `E.onException` failureAction

  b <- isWindowsMsys
  if b
    then do
        (r, e, c) <- rawShellSystemStdInOut normal "wx-config" parms
        if c == ExitSuccess then
          return r
        else runExecutable $ do
              putStrLn $ "Error: Failed to execute wx-config\n" ++ e
              exitFailure
    else
        runExecutable $ return ""


 -- Try to find a compatible version of wxWidgets
-- (a version that can be handled by this version of wxHaskell)
findWxVersion :: IO (Maybe String)
findWxVersion =
  if buildOS == Windows
    -- The Windows port of wx-config doesn't let you specify a version, nor query the full version,
    -- accordingly we just check what version is installed (which is returned with --release)
    then checkCompatibility <$> readVersionWindows
    else findM (fmap isCompatible . readVersion) wxCompatibleVersions
      where
        readVersionWindows :: IO String
        readVersionWindows =
          wx_config ["--version"]                          -- Sample output: 3.0.1

        readVersion :: String -> IO String
        readVersion x =
          wx_config ["--version=" ++ x, "--version-full"]  -- Sample output: 3.0.1.0

        isCompatible :: String -> Bool
        isCompatible xs =
          any (`isPrefixOf` xs) wxCompatibleVersions

        checkCompatibility :: String -> Maybe String
        checkCompatibility version =
          if isCompatible version
            then Just version
            else Nothing


parseWxConfig :: String -> BuildInfo
parseWxConfig s =
    helper emptyBuildInfo (words s)
  where
    helper b ("-framework":w:ws) = helper (b { frameworks = w : frameworks b }) ws
    helper b (w:ws)              = helper (f b w) ws
    helper b []                  = b
    f b w =
        case w of
          ('-':'L':v) -> b { extraLibDirs = v : extraLibDirs b }
          ('-':'l':v) -> b { extraLibs    = v : extraLibs b }
          ('-':'I':v) -> b { includeDirs  = v : includeDirs b }
          ('-':'D':_) -> b { ccOptions    = w : ccOptions b }
          _           -> b

deMsysPaths :: BuildInfo -> IO BuildInfo
deMsysPaths bi = do
    b <- isWindowsMsys
    if b
    then do
        let cor ph = do
            (IODataText r, e, c ) <- rawSystemStdInOut normal "sh" ["-c", "cd " ++ ph ++ "; pwd -W"] Nothing Nothing Nothing IODataModeText
            unless (c == ExitSuccess) (putStrLn ("Error: failed to convert MSYS path to native path \n" ++ e) >> exitFailure)
            return . head . lines $ r
        elds  <- mapM cor (extraLibDirs bi)
        incds <- mapM cor (includeDirs bi)
        return $ bi {extraLibDirs = elds, includeDirs = incds}
    else
        return bi

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Extend the standard build hook to build a shared library for wxc - this will statically link
-- any libraries which are unavailable as shared variants. This is mainly a work-around for the
-- fact that GHCi needs to load shared libraries at run-time, and that the Windows MinGW environment
-- is shipped with only a static version of libstdc++.
-- TODO: Does not currently create the build output directory.
myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook pkg_descr local_bld_info user_hooks bld_flags =
    do
    -- Extract the custom fields customFieldsPD where field name is x-dll-sources
    let lib       = fromJust (library pkg_descr)
        lib_bi    = libBuildInfo lib
        custom_bi = customFieldsBI lib_bi
        dll_name  = fromJust (lookup "x-dll-name" custom_bi)
        dll_srcs  = (lines . fromJust) (lookup "x-dll-sources" custom_bi)
        dll_libs  = (lines . fromJust) (lookup "x-dll-extra-libraries" custom_bi)
        cc_opts   = ccOptions lib_bi
        ld_opts   = ldOptions lib_bi
        inc_dirs  = includeDirs lib_bi
        lib_dirs  = extraLibDirs lib_bi
        libs      = extraLibs lib_bi
        bld_dir   = buildDir local_bld_info
        progs     = withPrograms local_bld_info
        gcc       = fromJust (lookupProgram (simpleProgram "gcc") progs)
        ver       = (pkgVersion . package) pkg_descr
        inst_lib_dir = libdir $ absoluteInstallDirs pkg_descr local_bld_info NoCopyDest
    -- Compile C/C++ sources - output directory is dist/build/src/cpp
    putStrLn "Building wxc"
    objs <- mapM (compileCxx gcc cc_opts inc_dirs bld_dir) dll_srcs

    -- Link C/C++ sources as a DLL - output directory is dist/build
    if buildOS == Windows then do
        -- Since we removed wx libraries in myConfHook we need to add them here when linking wxc.dll
        wx <- fmap parseWxConfig (checkWxVersion >>= readWxConfig) >>= deMsysPaths
        linkSharedLib gcc ld_opts lib_dirs (libs ++ reverse (extraLibs wx) ++ dll_libs) objs ver bld_dir dll_name inst_lib_dir
    else
        linkSharedLib gcc ld_opts lib_dirs (libs ++ dll_libs) objs ver bld_dir dll_name inst_lib_dir
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Return any compiler options required to support shared library creation
osCompileOpts :: [String] -- ^ Platform-specific compile options
osCompileOpts =
    case buildOS of
      Windows -> ["-DBUILD_DLL"]
      OSX     -> ["-fPIC"]
      _       -> ["-fPIC"]

sharedLibName :: Version -- ^ Version information to be used for Unix shared libraries
              -> String  -- ^ Name of the shared library
              -> String
sharedLibName ver basename =
    case buildOS of
      Windows -> addExtension basename ".dll"
      OSX     -> "lib" ++ addExtension basename ".dylib"
      _       -> "lib" ++ basename ++ ".so." ++ full_ver
        where
          full_ver = prettyShow ver

-- | Return any linker options required to support shared library creation
linkCxxOpts :: Version  -- ^ Version information to be used for Unix shared libraries
            -> FilePath -- ^ Directory in which library will be built
            -> String   -- ^ Name of the shared library
            -> String   -- ^ Absolute path of the shared library
            -> [String] -- ^ List of options which can be applied to 'runProgram'
linkCxxOpts ver out_dir basename basepath =
    case buildOS of
      Windows -> ["-Wl,--dll", "-shared",
                  "-o", out_dir </> sharedLibName ver basename,
                  "-Wl,--out-implib," ++ "lib" ++ addExtension basename ".a",
                  "-Wl,--export-all-symbols", "-Wl,--enable-auto-import",
                  "-Wl,-no-undefined,--enable-runtime-pseudo-reloc"]
      OSX     -> ["-dynamiclib",
                  "-o", out_dir </> sharedLibName ver basename,
                  "-install_name", basepath </> sharedLibName ver basename,
                  "-Wl,-undefined,dynamic_lookup"]
      _       -> ["-shared",
                  "-Wl,-soname,lib" ++ basename ++ ".so",
                  "-o", out_dir </> sharedLibName ver basename]

-- | Compile a single source file using the configured gcc, if the object file does not yet
-- exist, or is older than the source file.
-- TODO: Does not do dependency resolution properly
compileCxx :: ConfiguredProgram -- ^ Program used to perform C/C++ compilation (gcc)
           -> [String]          -- ^ Compile options provided by Cabal and wx-config
           -> [String]          -- ^ Include paths provided by Cabal and wx-config
           -> FilePath          -- ^ Base output directory
           -> FilePath          -- ^ Path to source file
           -> IO FilePath       -- ^ Path to generated object code
compileCxx gcc opts incls out_path cxx_src =
    do
    let includes  = map ("-I" ++) incls
        out_path' = normalisePath out_path
        cxx_src'  = normalisePath cxx_src
        out_file  = out_path' </> dropFileName cxx_src </> replaceExtension (takeFileName cxx_src) ".o"
        out       = ["-c", cxx_src', "-o", out_file]
        opts'     = opts ++ osCompileOpts
    do_it <- needsCompiling cxx_src out_file
    when do_it $ createDirectoryIfMissing True (dropFileName out_file) >>
                 runProgram verbose gcc (includes ++ opts' ++ out)
    return out_file

-- | Return True if obj does not exist or is older than src.
-- Real dependency checking would be nice here...
needsCompiling :: FilePath -- ^ Path to source file
               -> FilePath -- ^ Path to object file
               -> IO Bool  -- ^ True if compilation required
needsCompiling src obj =
    do
    has_obj <- doesFileExist obj
    if has_obj
        then do
          mtime_src <- getModificationTime src
          mtime_obj <- getModificationTime obj
          if mtime_obj < mtime_src then return True else return False
        else
          return True

-- | Create a dynamically linked library using the configured ld.
linkSharedLib :: ConfiguredProgram -- ^ Program used to perform linking
              -> [String]          -- ^ Linker options supplied by Cabal
              -> [FilePath]        -- ^ Library directories
              -> [String]          -- ^ Libraries
              -> [String]          -- ^ Objects
              -> Version           -- ^ wxCore version (wxC has same version number)
              -> FilePath          -- ^ Directory in which library will be generated
              -> String            -- ^ Name of the shared library
              -> String            -- ^ Absolute path of the shared library
              -> IO ()
linkSharedLib gcc opts lib_dirs libs objs ver out_dir dll_name dll_path =
    do
    let lib_dirs' = map (\d -> "-L" ++ normalisePath d) lib_dirs
        out_dir'  = normalisePath out_dir
        opts'     = opts ++ linkCxxOpts ver out_dir' dll_name dll_path
        objs'     = map normalisePath objs
        libs'     = ["-lstdc++"] ++ map ("-l" ++) libs
        target    = out_dir' </> sharedLibName ver dll_name
    link <- linkingNeeded target objs'
    when link $
      do
        putStrLn "Linking wxc"
        runProgram verbose gcc (opts' ++ objs' ++ lib_dirs' ++ libs')


-- | Check if one of the input files is more recent then the output file
linkingNeeded :: FilePath -> [FilePath] -> IO Bool
linkingNeeded output input =
  do
    fileExists <- doesFileExist output
    if not fileExists
      then return True
      else
        do
          mostRecentModificationTime <- maximum <$> mapM getModificationTime input
          outputModificationTime     <- getModificationTime output
          return $ mostRecentModificationTime > outputModificationTime


-- | The 'normalise' implementation in System.FilePath does not meet the requirements of
-- calling and/or running external programs on Windows particularly well as it does not
-- normalise the '/' character to '\\'. The problem is that some MinGW programs do not
-- like to see paths with a mixture of '/' and '\\'. Sine we are calling out to these,
-- we require a stricter normalisation.
normalisePath :: FilePath -> FilePath
normalisePath = case buildOS of
                  Windows -> dosifyFilePath
                  _       -> unixifyFilePath

-- | Replace a character in a String with some other character
replace :: Char   -- ^ Character to replace
        -> Char   -- ^ Character with which to replace
        -> String -- ^ String in which to replace
        -> String -- ^ Transformed string
replace old new = map replace'
    where
      replace' elem = if elem == old then new else elem

unixifyFilePath = replace '\\' '/'
dosifyFilePath = replace '/' '\\'

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Run ldconfig in `path` and return a list of all the links which were created
ldconfig :: FilePath -> IO ()
ldconfig path = case buildOS of
    Windows -> return ()
    OSX     -> return ()
    _       ->
          do
            ld_exit_code <- system ("/sbin/ldconfig -n " ++ path)
            case ld_exit_code of
                ExitSuccess -> return ()
                otherwise -> error "Couldn't execute ldconfig, ensure it is on your path"

myCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
myCopyHook = hookHelper (fromFlag . copyVerbosity) (fromFlagOrDefault NoCopyDest . copyDest) (copyHook simpleUserHooks)

myInstHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
myInstHook = hookHelper (fromFlag . installVerbosity) (const NoCopyDest) (instHook simpleUserHooks)

hookHelper ::
    (a -> Verbosity) ->
    (a -> CopyDest)  ->
    (PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()) ->
    PackageDescription -> LocalBuildInfo -> UserHooks -> a -> IO ()
hookHelper verbosity copydest origHook pkg_descr local_bld_info user_hooks flags =
    do
    -- Perform simpleUserHooks (copyHook/instHook => to copy installIncludes)
    origHook pkg_descr local_bld_info user_hooks flags

    -- Copy shared library
    let bld_dir      = buildDir local_bld_info

        ver          = (pkgVersion . package) pkg_descr
        lib          = fromJust (library pkg_descr)
        lib_bi       = libBuildInfo lib
        custom_bi    = customFieldsBI lib_bi
        dll_name     = fromJust (lookup "x-dll-name" custom_bi)
        lib_name     = sharedLibName ver dll_name

        inst_lib_dir = libdir $ absoluteInstallDirs pkg_descr local_bld_info (copydest flags)

    installOrdinaryFile (verbosity flags) (bld_dir </> lib_name) (inst_lib_dir </> lib_name)
    ldconfig inst_lib_dir
