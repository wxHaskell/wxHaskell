-----------------------------------------------------------------------------------------
{-| Module      :  CompileClassTypes
    Copyright   :  (c) Daan Leijen 2003, 2004
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Module that compiles classes to class type definitions to Haskell.
-}
-----------------------------------------------------------------------------------------
module CompileClassTypes( compileClassTypes ) where

import qualified Data.Map as Map

-- import Data.Time( getCurrentTime)
-- import Types
import HaskellNames
import Classes( {-isClassName,-} haskellClassDefs )
import DeriveTypes( ClassName )
import IOExtra

{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileClassTypes :: Bool -> String -> String -> FilePath -> [FilePath] -> IO ()
compileClassTypes _showIgnore moduleRoot moduleName outputFile inputFiles
  = do -- time    <- getCurrentTime
       let (exportsClass,classDecls) = haskellClassDefs
           exportsClassClasses       = exportDefs exportsClass 

           classCount   = length exportsClass
           
           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Classes" ]
                              , exportsClassClasses
                              , [ "    ) where"
                                , ""
                                , "import " ++ moduleRoot ++ "WxcObject"
                                , "" ]
                              ]

           prologue = getPrologue moduleName "class"
                                   (show classCount ++ " class definitions.")
                                   inputFiles
           output = unlines (prologue ++ export ++ classDecls)

       putStrLn ("generating: " ++ outputFile)
       writeFileLazy outputFile output
       putStrLn ("generated " ++ show classCount ++ " class definitions.")
       putStrLn ("ok.")


{-----------------------------------------------------------------------------------------
   Create export definitions
-----------------------------------------------------------------------------------------}
exportDefs :: [(ClassName,[String])] -> [String]
exportDefs classExports 
  = let classMap = Map.fromListWith (++) classExports         
    in  concatMap exportDef $ zip [(0 :: Int)..] (Map.toAscList classMap)
  where
    exportDef (n, (className,exports))
      = [heading 2 className] ++ (commaSep n) exports

    commaSep n xs
      = zipWith (exportComma n) [(0 :: Int)..] xs

    heading i name
      = exportSpaces ++ "-- " ++ replicate i '*' ++ " " ++ name

    exportComma n m str = exportSpaces
                          ++ (if n == 0 && m == 0 then " " else ",")
                          ++ str
-- To prevent messages like "Defined but not used: data constructor `CConfigBase'":
                          ++ (if m == 2 then "(..)" else "") 
    
    exportSpaces = "     "

