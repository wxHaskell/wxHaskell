
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

{- cabal:
build-depends:
  base,
  Cabal,
  directory,
  filepath,
  process,
  split,
-}


module Main (main) where

import Control.Exception (onException)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Distribution.Simple.Utils ( IODataMode(..), rawSystemStdInOut)
import Distribution.System (OS (..), buildOS)
import Distribution.Verbosity (Verbosity, normal)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.Process (readProcess)

main :: IO ()
main = generateHeaders =<< checkWxVersion


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- Some utility functions


-- Find the first element in a list that matches a condition
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _  []       = return Nothing
findM mp (x : xs) =
  do
    r <- mp x
    if r
      then return $ Just x
      else findM mp xs


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

rawShellSystemStdInOut :: Verbosity                     -- Verbosity level
                       -> FilePath                      -- Path to command
                       -> [String]                      -- Command arguments
                       -> IO (String, String, ExitCode) -- (Command result, Errors, Command exit status)
rawShellSystemStdInOut v f as =
    rawSystemStdInOut v "sh" (f:as) Nothing Nothing Nothing IODataModeText

isWindowsMsys :: IO Bool
isWindowsMsys = (buildOS == Windows&&) . isJust <$> lookupEnv "MSYSTEM"


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

generateHeaders :: String -> IO ()
generateHeaders wxVersion =
  do
    writeFile ("src" </> "include" </> "wxc_def.h")
      $ cppDefinition "wxVERSION_NUMBER" (wxVersionNumber wxVersion)

cppDefinition :: Show a => String -> a -> String
cppDefinition name value =
  "#define " ++ name ++ " " ++ show value ++ "\n"

wxVersionNumber :: String -> Int
wxVersionNumber wxVersion =
  let (major : minor : remaining) = splitOn "." wxVersion in
    read major * 1000 + read minor * 100 +
      case remaining of
        [] -> 0
        [revision] -> read revision
        _ -> error "Failed to read version number"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


-- A list of wxWidgets versions that can be handled by this version of wxHaskell
wxCompatibleVersions :: [String]
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

wxConfig :: [String] -> IO String
wxConfig parms = do
  let runExecutable failureAction =
        readProcess "wx-config" parms "" `onException` failureAction

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
          wxConfig ["--version"]                          -- Sample output: 3.0.1

        readVersion :: String -> IO String
        readVersion x =
          wxConfig ["--version=" ++ x, "--version-full"]  -- Sample output: 3.0.1.0

        isCompatible :: String -> Bool
        isCompatible xs =
          any (`isPrefixOf` xs) wxCompatibleVersions

        checkCompatibility :: String -> Maybe String
        checkCompatibility version =
          if isCompatible version
            then Just version
            else Nothing
