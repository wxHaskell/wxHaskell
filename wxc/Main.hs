{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

{- cabal:
build-depends:
  base,
  extra,
  process,
  split,
-}

module Main (main) where

import Control.Monad.Extra
import Data.Function
import Data.List
import Data.List.Split
import System.Process

-- A list of wxWidgets versions that can be handled by this version of wxHaskell
wxCompatibleVersions :: [String]
wxCompatibleVersions = ["3.0", "2.9"] -- Preferred version first

main :: IO ()
main = do
  -- Try to find a compatible version of wxWidgets
  -- (a version that can be handled by this version of wxHaskell)
  maybeVersion <-
    wxCompatibleVersions & findM \x -> do
      v <- readProcess "wx-config" ["--version=" ++ x, "--version-full"] ""
      pure $ any (`isPrefixOf` v) wxCompatibleVersions
  case maybeVersion of
    Nothing ->
      error $
        "This version of wxc requires one of the following wxWidgets versions to be available: "
          ++ show wxCompatibleVersions
    Just version ->
      writeFile "src/include/wxc_def.h" $
        "#define wxVERSION_NUMBER " ++ wxVersionNumber ++ "\n"
     where
      wxVersionNumber =
        let (major : minor : remaining) = splitOn "." version
         in major ++ minor ++ pad 2 '0' case remaining of
              [] -> ""
              [revision] -> revision
              _ -> error "too many dots in version number"

pad :: Int -> a -> [a] -> [a]
pad n c xs = replicate (n - length xs) c ++ xs
