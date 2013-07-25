import Control.Monad (mapM_, when)
import Data.List (foldl', intersperse, intercalate, nub, lookup, isPrefixOf)
import Data.Maybe (fromJust)
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.InstallDirs (InstallDirs(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, localPkgDescr, installedPkgs, withPrograms, buildDir, absoluteInstallDirs)
import Distribution.Simple.PackageIndex(SearchResult (..), searchByName )
import Distribution.Simple.Program (ConfiguredProgram (..), lookupProgram, runProgram, simpleProgram, locationPath)
import Distribution.Simple.Setup (ConfigFlags, BuildFlags, InstallFlags, CopyDest(..), fromFlag, installVerbosity)
import Distribution.Simple.Utils (installOrdinaryFile)
import Distribution.System (OS (..), Arch (..), buildOS, buildArch)
import Distribution.Verbosity (normal, verbose)
import System.Cmd (system)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getModificationTime)
import System.Environment (getEnv)
import System.Exit (ExitCode (..))
import System.FilePath.Posix ((</>), (<.>), replaceExtension, takeFileName, dropFileName, addExtension)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Process as Process
import qualified Control.Exception as E

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

readProcess :: FilePath -> [String] -> String -> IO String
readProcess cmd args stdin =
  Process.readProcess cmd args stdin
  `E.catch` \(E.SomeException err) -> do
    hPutStrLn stderr $ "readProcess failed: " ++ show err
    E.throwIO err

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = myConfHook, buildHook = myBuildHook, instHook = myInstHook }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

sourceDirectory  :: FilePath
includeDirectory :: FilePath

sourceDirectory  = "cpp"
includeDirectory = "include"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Comment out type signature because of a Cabal API change from 1.6 to 1.7
myConfHook (pkg0, pbi) flags = do

    lbi <- confHook simpleUserHooks (pkg0, pbi) flags
    let lpd       = localPkgDescr lbi
    let lib       = fromJust (library lpd)
    let libbi     = libBuildInfo lib
    let custom_bi = customFieldsBI libbi

    wx <- fmap parseWxConfig readWxConfig

    let libbi' = libbi
          { extraLibDirs = extraLibDirs libbi ++ extraLibDirs wx
          , extraLibs    = extraLibs    libbi ++ reverse (extraLibs    wx)
          , ldOptions    = ldOptions    libbi ++ ldOptions    wx
          , frameworks   = frameworks   libbi ++ frameworks   wx
          , includeDirs  = includeDirs  libbi ++ includeDirs  wx
          , ccOptions    = ccOptions    libbi ++ ccOptions    wx ++ ["-DwxcREFUSE_MEDIACTRL"]
          }

    let lib' = lib { libBuildInfo = libbi' }
    let lpd' = lpd { library = Just lib' }

    return $ lbi { localPkgDescr = lpd' }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Make sure the right version of wx widgets is installed and return the 
readWxConfig :: IO String
readWxConfig = do
    -- Try to force version and see if we have it
    let wxRequiredVersion = "2.9"

    -- The Windows port of wx-config doesn't let you specify a version, nor query the full version,
    -- accordingly we just check what version is installed (which is returned with --release)
    wxVersion <- case buildOS of
      Windows -> readProcess "wx-config" ["--release"] ""
      _       -> readProcess "wx-config" ["--version=" ++ wxRequiredVersion, "--version-full"] ""

    if wxRequiredVersion `isPrefixOf` wxVersion 
      then putStrLn ("Configuring wxc to build against wxWidgets " ++ wxVersion)
      else error ("This version of wxc requires wxWidgets " ++ wxRequiredVersion ++ " to be available")

    -- The Windows port of wx-config doesn't let you specify a version (yet)
    output <- case buildOS of
      -- TODO: Nasty Windows hack: wx-config-win barfs if --cppflags comes after --libs :-(
      Windows -> readProcess "wx-config" ["--cppflags", "--libs", "all"] ""
      _       -> readProcess "wx-config" ["--version=" ++ wxRequiredVersion, "--libs", "all", "--cppflags"] ""
    return output

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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Extend the standard build hook to build a shared library for wxc - this will statically link
-- any libraries which are unavailable as shared variants. This is mainly a work-around for the
-- fact that GHCi needs to load shared libraries at run-time, and that the Windows MinGW environment
-- is shipped with only a static version of libstdc++.
-- TODO: Does not currently create the build output directory.
myBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
myBuildHook pkg_descr local_bld_info user_hooks bld_flags =
    do
    -- Extract the custom fields customFieldsPD where field name is x-cpp-dll-sources
    let lib = fromJust (library pkg_descr)
        lib_bi = libBuildInfo lib
        custom_bi = customFieldsBI lib_bi
        dll_name = fromJust (lookup "x-dll-name" custom_bi)
        dll_srcs = (lines . fromJust) (lookup "x-dll-sources" custom_bi)
        dll_libs = (lines . fromJust) (lookup "x-dll-extra-libraries" custom_bi)
        cc_opts = ccOptions lib_bi
        ld_opts = ldOptions lib_bi
        inc_dirs = includeDirs lib_bi
        lib_dirs = extraLibDirs lib_bi
        libs = extraLibs lib_bi
        bld_dir = buildDir local_bld_info
        progs = withPrograms local_bld_info
        gcc = fromJust (lookupProgram (simpleProgram "gcc") progs)
        ver = (pkgVersion . package) pkg_descr
        inst_lib_dir = libdir $ absoluteInstallDirs pkg_descr local_bld_info NoCopyDest
    -- Compile C/C++ sources - output directory is dist/build/src/cpp
    putStrLn "Building wxc"
    objs <- mapM (compileCxx gcc cc_opts inc_dirs bld_dir) dll_srcs
    -- Link C/C++ sources as a DLL - output directory is dist/build
    putStrLn "Linking wxc"
    linkSharedLib gcc ld_opts lib_dirs (libs ++ dll_libs) objs ver bld_dir dll_name inst_lib_dir

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | Return any compiler options required to support shared library creation
osCompileOpts :: [String] -- ^ Platform-specific compile options
osCompileOpts =
    case buildOS of
      Windows -> ["-DBUILD_DLL"]
      OSX -> ["-fPIC"]
      _ -> ["-fPIC"]

sharedLibName :: Version -- ^ Version information to be used for Unix shared libraries
              -> String -- ^ Name of the shared library
              -> String
sharedLibName ver basename =
    case buildOS of
      Windows -> addExtension basename ".dll"
      OSX -> "lib" ++ addExtension basename ".dylib"
      _ -> "lib" ++ basename ++ ".so." ++ full_ver
        where
          full_ver = (concat . intersperse "." . map show . versionBranch) ver

-- | Return any linker options required to support shared library creation
linkCxxOpts :: Version -- ^ Version information to be used for Unix shared libraries
            -> FilePath -- ^ Directory in which library will be built
            -> String -- ^ Name of the shared library
            -> String -- ^ Absolute path of the shared library
            -> [String] -- ^ List of options which can be applied to 'runProgram'
linkCxxOpts ver out_dir basename basepath =
    let dll_pathname = normalisePath (out_dir </> addExtension basename ".dll")
        implib_pathname = normalisePath (out_dir </> "lib" ++ addExtension basename ".a") in
    case buildOS of
      Windows -> ["--dll", "-shared", 
                  "-o", out_dir </> sharedLibName ver basename,
                  "-Wl,--out-implib," ++ "lib" ++ addExtension basename ".a",
                  "-Wl,--export-all-symbols", "-Wl,--enable-auto-import"]
      OSX -> ["-dynamiclib",
                  "-o", out_dir </> sharedLibName ver basename,
                  "-install_name", basepath </> sharedLibName ver basename,
                  "-Wl,-undefined,dynamic_lookup"]
      _ -> ["-shared",
                  "-Wl,-soname,lib" ++ basename ++ ".so",
                  "-o", out_dir </> sharedLibName ver basename]

-- | Compile a single source file using the configured gcc, if the object file does not yet
-- exist, or is older than the source file.
-- TODO: Does not do dependency resolution properly
compileCxx :: ConfiguredProgram -- ^ Program used to perform C/C++ compilation (gcc)
           -> [String] -- ^ Compile options provided by Cabal and wxConfig
           -> [String] -- ^ Include paths provided by Cabal and wxConfig
           -> FilePath -- ^ Base output directory
           -> FilePath -- ^ Path to source file
           -> IO FilePath -- ^ Path to generated object code
compileCxx gcc opts incls out_path cxx_src =
    do
    let includes = map ("-I" ++) incls
        out_path' = normalisePath out_path
        cxx_src' = normalisePath cxx_src
        out_file = out_path' </> dropFileName cxx_src </> replaceExtension (takeFileName cxx_src) ".o"
        out = ["-c", cxx_src', "-o", out_file]
        opts' = opts ++ osCompileOpts
    do_it <- needsCompiling cxx_src out_file
    when do_it $ createDirectoryIfMissing True (dropFileName out_file) >>
                 runProgram verbose gcc (includes ++ opts' ++ out)
    return out_file

-- | Return True if obj does not exist or is older than src.
-- Real dependency checking would be nice here...
needsCompiling :: FilePath -- ^ Path to source file
               -> FilePath -- ^ Path to object file
               -> IO Bool -- ^ True if compilation required
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
              -> [String] -- ^ Linker options supplied by Cabal
              -> [FilePath] -- ^ Library directories
              -> [String] -- ^ Libraries
              -> [String] -- ^ Objects
              -> Version -- ^ wxCore version (wxC has same version number)
              -> FilePath -- ^ Directory in which library will be generated
              -> String -- ^ Name of the shared library
              -> String -- ^ Absolute path of the shared library
              -> IO ()
linkSharedLib gcc opts lib_dirs libs objs ver out_dir dll_name dll_path =
    do
    let lib_dirs' = map (\d -> "-L" ++ normalisePath d) lib_dirs
        out_dir' = normalisePath out_dir
        opts' = opts ++ linkCxxOpts ver (out_dir') dll_name dll_path
        objs' = map normalisePath objs
        libs' = ["-lstdc++"] ++ map ("-l" ++) libs
    runProgram verbose gcc (opts' ++ objs' ++ lib_dirs' ++ libs')
    --system $ (unwords ([show . locationPath . programLocation $ gcc] ++ opts' ++ objs' ++ lib_dirs' ++ libs'))
    return ()

-- | The 'normalise' implementation in System.FilePath does not meet the requirements of
-- calling and/or running external programs on Windows particularly well as it does not
-- normalise the '/' character to '\\'. The problem is that some MinGW programs do not
-- like to see paths with a mixture of '/' and '\\'. Sine we are calling out to these,
-- we require a stricter normalisation.
normalisePath :: FilePath -> FilePath
normalisePath = case buildOS of
                  Windows -> dosifyFilePath
                  _ -> unixifyFilePath

-- | Replace a character in a String with some other character
replace :: Char -- ^ Character to replace
        -> Char -- ^ Character with which to replace
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
    OSX -> return ()
    _ -> do
            ld_exit_code <- system ("/sbin/ldconfig -n " ++ path) 
            case ld_exit_code of
                ExitSuccess -> return ()
                otherwise -> error "Couldn't execute ldconfig, ensure it is on your path"

myInstHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
myInstHook pkg_descr local_bld_info user_hooks inst_flags = 
    do
    -- Perform simpleUserHooks instHook (to copy installIncludes)
    instHook simpleUserHooks pkg_descr local_bld_info user_hooks inst_flags

    -- Copy shared library
    let bld_dir = buildDir local_bld_info

        ver = (pkgVersion . package) pkg_descr
        lib = fromJust (library pkg_descr)
        lib_bi = libBuildInfo lib
        custom_bi = customFieldsBI lib_bi
        dll_name = fromJust (lookup "x-dll-name" custom_bi)
        lib_name = sharedLibName ver dll_name

        inst_lib_dir = libdir $ absoluteInstallDirs pkg_descr local_bld_info NoCopyDest

    installOrdinaryFile (fromFlag (installVerbosity inst_flags)) (bld_dir </> lib_name) (inst_lib_dir </> lib_name)
    ldconfig inst_lib_dir

