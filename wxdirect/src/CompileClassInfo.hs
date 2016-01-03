-----------------------------------------------------------------------------------------
{-| Module      :  CompileClassInfo
    Copyright   :  (c) Daan Leijen 2003, 2004
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Module that compiles class types to a Haskell module for safe casting
-}
-----------------------------------------------------------------------------------------
module CompileClassInfo( compileClassInfo ) where

import Data.Char( toLower )
import Data.List( sortBy {-, sort-} )

-- import Types
import HaskellNames
import Classes
import IOExtra


{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileClassInfo :: Bool -> String -> String -> String -> String -> FilePath -> IO ()
compileClassInfo _verbose moduleRoot moduleClassesName moduleClassTypesName moduleName outputFile
  = do let classNames'  = sortBy cmpName objectClassNames
           (classExports,classDefs) = unzip (map toHaskellClassType classNames') 
           (downcExports,downcDefs) = unzip (map toHaskellDowncast classNames')

           defCount = length classNames'

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Class Info"
                                , "      ClassType, classInfo, instanceOf, instanceOfName"
                                , "      -- * Safe casts"
                                , "    , safeCast, ifInstanceOf, whenInstanceOf, whenValidInstanceOf"
                                , "      -- * Class Types"
                                ]
                              , map (exportComma++) classExports
                              , [ "      -- * Down casts" ]
                              , map (exportComma++) downcExports
                              , [ "    ) where"
                                , ""
                                , "import System.IO.Unsafe( unsafePerformIO )"
                                , "import " ++ moduleRoot ++ moduleClassTypesName
                                , "import " ++ moduleRoot ++ "WxcTypes"
                                , "import " ++ moduleRoot ++ moduleClassesName
                                , ""
                                , "-- | The type of a class."
                                , "data ClassType a = ClassType (ClassInfo ())"
                                , ""
                                , "-- | Return the 'ClassInfo' belonging to a class type. (Do not delete this object, it is statically allocated)"
                                , "{-# NOINLINE classInfo #-}"
                                , "classInfo :: ClassType a -> ClassInfo ()"
                                , "classInfo (ClassType info) = info"
                                , ""
                                , "-- | Test if an object is of a certain kind. (Returns also 'True' when the object is null.)"
                                , "{-# NOINLINE instanceOf #-}"
                                , "instanceOf :: WxObject b -> ClassType a -> Bool"
                                , "instanceOf obj (ClassType classInfo') "
                                , "  = if (objectIsNull obj)"
                                , "     then True"
                                , "     else unsafePerformIO (objectIsKindOf obj classInfo')"
                                , ""
                                , "-- | Test if an object is of a certain kind, based on a full wxWidgets class name. (Use with care)."
                                , "{-# NOINLINE instanceOfName #-}"
                                , "instanceOfName :: WxObject a -> String -> Bool"
                                , "instanceOfName obj className "
                                , "  = if (objectIsNull obj)"
                                , "     then True"
                                , "     else unsafePerformIO ("
                                , "          do classInfo'   <- classInfoFindClass className"
                                , "             if (objectIsNull classInfo')"
                                , "              then return False" 
                                , "              else objectIsKindOf obj classInfo')"
                                , ""
                                , "-- | A safe object cast. Returns 'Nothing' if the object is of the wrong type. Note that a null object can always be cast."
                                , "safeCast :: WxObject b -> ClassType (WxObject a) -> Maybe (WxObject a)"
                                , "safeCast obj classType"
                                , "  | instanceOf obj classType = Just (objectCast obj)"
                                , "  | otherwise                = Nothing"
                                , ""
                                , "-- | Perform an action when the object has the right type /and/ is not null."
                                , "whenValidInstanceOf :: WxObject a -> ClassType (WxObject b) -> (WxObject b -> IO ()) -> IO ()"
                                , "whenValidInstanceOf obj classType f"
                                , "  = whenInstanceOf obj classType $ \\object ->"
                                , "    if (object==objectNull) then return () else f object"
                                , ""
                                , "-- | Perform an action when the object has the right kind. Note that a null object has always the right kind."
                                , "whenInstanceOf :: WxObject a -> ClassType (WxObject b) -> (WxObject b -> IO ()) -> IO ()"
                                , "whenInstanceOf obj classType f"
                                , "  = ifInstanceOf obj classType f (return ())"
                                , ""
                                , "-- | Perform an action when the object has the right kind. Perform the default action if the kind is not correct. Note that a null object has always the right kind."
                                , "ifInstanceOf :: WxObject a -> ClassType (WxObject b) -> (WxObject b -> c) -> c -> c"
                                , "ifInstanceOf obj classType yes no"
                                , "  = case safeCast obj classType of"
                                , "      Just object -> yes object"
                                , "      Nothing     -> no"
                                , ""
                                ]
                              ]
           prologue = getPrologue moduleName "class info"
                                        (show defCount ++ " class info definitions.") []
           haddockPrune = ["{-# OPTIONS_HADDOCK prune #-}"]

       putStrLn ("generating: " ++ outputFile)
       writeFileLazy outputFile (unlines (haddockPrune ++ prologue ++ export ++ classDefs ++ downcDefs))
       putStrLn ("generated " ++ show defCount ++ " class info definitions")
       putStrLn "ok."

cmpName :: String -> String -> Ordering
cmpName s1 s2
  = compare (map toLower (haskellTypeName s1)) (map toLower (haskellTypeName s2))

{-
cmpDef :: Def -> Def -> Ordering
cmpDef def1 def2
  = compare (defName def1) (defName def2)
-}

exportComma :: String
exportComma  = exportSpaces ++ ","

exportSpaces :: String
exportSpaces = "     "


{-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------}
toHaskellClassType :: String -> (String,String)
toHaskellClassType className
  = (classTypeDeclName
    ,"{-# NOINLINE " ++ classTypeDeclName ++ " #-}\n" ++
     classTypeDeclName ++ " :: ClassType (" ++ classTypeName' ++ " ())\n" ++
     classTypeDeclName ++ " = ClassType (unsafePerformIO (classInfoFindClass " ++ classTypeString ++ "))\n\n"
    )
  where
    classTypeDeclName :: String
    classTypeDeclName = haskellDeclName ("class" ++ classTypeName')
    classTypeName'    :: String
    classTypeName'    = haskellTypeName className
    classTypeString   :: String
    classTypeString   = "\"" ++ className ++ "\""


{-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------}
toHaskellDowncast :: String -> (String,String)
toHaskellDowncast className
  = (downcastName
    ,downcastName ++ " :: " ++ classTypeName' ++ " a -> " ++ classTypeName' ++ " ()\n" ++
     downcastName ++ " obj = objectCast obj\n\n"
    )
  where
    classTypeName'    = haskellTypeName className
    downcastName      = haskellDeclName ("downcast" ++ classTypeName')
