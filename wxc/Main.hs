{-# OPTIONS_GHC -Wall #-}

{- cabal:
build-depends:
  base,
  extra,
  filepath,
  process,
  split,
-}

module Main (main) where

import Control.Monad.Extra
import Data.Functor
import Data.List
import Data.List.Split
import System.FilePath
import System.Info.Extra
import System.Process

main :: IO ()
main = do
  maybeVersion <- findWxVersion
  case maybeVersion of
    Nothing ->
      error $
        "This version of wxc requires one of the following wxWidgets versions to be available: "
          ++ show wxCompatibleVersions
    Just version ->
      writeFile ("src" </> "include" </> "wxc_def.h") $
        "#define wxVERSION_NUMBER " ++ show (wxVersionNumber version) ++ "\n"

wxVersionNumber :: String -> Int
wxVersionNumber wxVersion =
  let (major : minor : remaining) = splitOn "." wxVersion
   in read major * 1000 + read minor * 100
        + case remaining of
          [] -> 0
          [revision] -> read revision
          _ -> error "Failed to read version number"

-- A list of wxWidgets versions that can be handled by this version of wxHaskell
wxCompatibleVersions :: [String]
wxCompatibleVersions = ["3.0", "2.9"] -- Preferred version first

wxConfig :: [String] -> IO String
wxConfig parms = readProcess "wx-config" parms ""

-- Try to find a compatible version of wxWidgets
-- (a version that can be handled by this version of wxHaskell)
findWxVersion :: IO (Maybe String)
findWxVersion =
  -- The Windows port of wx-config doesn't let you specify a version, nor query the full version,
  -- accordingly we just check what version is installed (which is returned with --release)
  if isWindows
    then readVersionWindows <&> \version -> guard (isCompatible version) $> version
    else findM (fmap isCompatible . readVersion) wxCompatibleVersions
 where
  readVersionWindows :: IO String
  readVersionWindows =
    wxConfig ["--version"] -- Sample output: 3.0.1
  readVersion :: String -> IO String
  readVersion x =
    wxConfig ["--version=" ++ x, "--version-full"] -- Sample output: 3.0.1.0
  isCompatible :: String -> Bool
  isCompatible xs =
    any (`isPrefixOf` xs) wxCompatibleVersions
