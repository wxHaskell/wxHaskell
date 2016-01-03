-----------------------------------------------------------------------------------------
{-| Module      :  CompileClasses
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Module that compiles method definitions to Haskell, together
    with a proper marshalling wrapper.
-}
-----------------------------------------------------------------------------------------
module CompileClasses( compileClasses, haskellTypeArg, haskellTypePar ) where

import qualified Data.Map as Map

import Data.Time( getCurrentTime )
import Data.Char( toLower )
import Data.List( isInfixOf, isPrefixOf, sort, sortBy, elemIndex )

import Types
import HaskellNames
import Classes( haskellClassDefs, objectClassNames, ClassInfo(..), classInfo, classIsManaged )
import ParseC( parseC )
import DeriveTypes( deriveTypes, classifyName, Name(..), Method(..), ClassName )
import IOExtra

{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileClasses :: Bool -> String -> String -> String -> FilePath -> [FilePath] -> IO ()
compileClasses showIgnore moduleRoot moduleClassTypesName moduleName outputFile inputFiles
  = do declss  <- mapM parseC inputFiles
       time    <- getCurrentTime
       let splitter        = 'M'
           (decls1,decls2) = let isLower decl = (haskellDeclName (declName decl) < [toLower splitter])
                             in span isLower (deriveTypes showIgnore (sortBy cmpDecl (concat declss)))

           postfix1       = "A" ++ [toEnum (fromEnum splitter -1)]
           postfix2       = [splitter] ++ "Z"

           module1        = moduleRoot ++ moduleName ++ postfix1
           module2        = moduleRoot ++ moduleName ++ postfix2

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Re-export"
                                , "      module " ++ module1
                                , "    , module " ++ module2
                                , "    , module " ++ moduleRoot ++ moduleClassTypesName
                                , "    ) where"
                                , ""
                                , "import " ++ module1
                                , "import " ++ module2
                                , "import " ++ moduleRoot ++ moduleClassTypesName
                                , ""
                                ]
                              ]

       (m1,c1) <- compileClassesFile showIgnore moduleRoot moduleClassTypesName
                                    (moduleName ++ postfix1) (outputFile ++ postfix1) inputFiles decls1 time
       (m2,c2) <- compileClassesFile showIgnore moduleRoot moduleClassTypesName
                                    (moduleName ++ postfix2) (outputFile ++ postfix2) inputFiles decls2 time
       let methodCount = m1 + m2
           classCount  = c1 + c2

           prologue = getPrologue moduleName "class"
                                   (show methodCount ++ " methods for " ++ show classCount ++ " classes.")
                                   inputFiles

       let output  = unlines (prologue ++ export)
       putStrLn ("generating: " ++ outputFile ++ ".hs")
       writeFileLazy (outputFile ++ ".hs") output
       putStrLn ("generated " ++ show methodCount ++ " total methods for " ++ show classCount ++ " total classes.")
       putStrLn ("ok.")


compileClassesFile :: a
                   -> String
                   -> String
                   -> String
                   -> String
                   -> [String]
                   -> [Decl]
                   -> b
                   -> IO (Int, Int)
compileClassesFile _showIgnore moduleRoot moduleClassTypesName moduleName outputFile inputFiles decls _time
  = do let foreignDecls = map foreignDecl decls
           haskellDecls = map haskellDecl decls
           typeDecls    = map haskellTypeDecl decls

           marshalDecls = concat (zipWith3 (\t h f -> [t,h,f,""]) typeDecls haskellDecls foreignDecls)

           (exportsClass, _classDecls) = haskellClassDefs

           (exportsStatic,exportsClassClasses,classCount) = exportDefs decls exportsClass []

           methodCount  = length decls
           ghcoptions   = [ "{-# LANGUAGE ForeignFunctionInterface #-}"]

           export   = concat  [ ["module " ++ moduleRoot ++ moduleName
                                , "    ( -- * Global" ]
                              , (let es1 : es2 : estail = exportsStatic in es1 : dropFirstComma es2 : estail)
                                , [ "      -- * Classes" ]
                              , exportsClassClasses
                              , [ "    ) where"
                                , ""
                                , "import Prelude hiding (id, last, length, lines, max, min, show)"
                              ]
                              , if any ("B.ByteString" `isInfixOf`) typeDecls
                                  then
                                    [ "import qualified Data.ByteString as B (ByteString, useAsCStringLen)"
                                    , "import qualified Data.ByteString.Lazy as LB (ByteString, length, unpack)"
                                    ]
                                  else []
                              , if any ("unsafePerformIO" `isInfixOf`) haskellDecls
                                  then ["import System.IO.Unsafe( unsafePerformIO )"]
                                  else []
                              , [ "import " ++ moduleRoot ++ "WxcTypes hiding (rect, rgb, rgba, sz)"
                                , "import " ++ moduleRoot ++ moduleClassTypesName
                                , ""
                                ]
                              ]

           prologue = getPrologue moduleName "class"
                                   (show methodCount ++ " methods for " ++ show classCount ++ " classes.")
                                   inputFiles
           haddockPrune = ["{-# OPTIONS_HADDOCK prune #-}"]
           output  = unlines (haddockPrune ++ ghcoptions ++ prologue ++ export ++ marshalDecls)

       putStrLn ("generating: " ++ outputFile ++ ".hs")
       writeFileLazy (outputFile ++ ".hs") output
       putStrLn ("generated " ++ show methodCount ++ " methods for " ++ show classCount ++ " classes.")
       return (methodCount,classCount)


cmpDecl :: Decl -> Decl -> Ordering
cmpDecl decl1 decl2
  = compare (haskellDeclName (declName decl1)) (haskellDeclName (declName decl2))

exportComma :: String
exportComma  = exportSpaces ++ ","

exportSpaces :: String
exportSpaces = "     "

dropFirstComma :: String -> String
dropFirstComma str = maybe str replaceNthWithSpace (elemIndex ',' str)
    where replaceNthWithSpace n = let (front,back) = splitAt n str in front ++ " " ++ tail back

{-----------------------------------------------------------------------------------------
   Create export definitions
-----------------------------------------------------------------------------------------}
exportDefs :: [Decl] -> [(ClassName,[String])] -> [(ClassName,[String])] -> ([String],[String],Int)
exportDefs decls classExports shortExports
  = let classMap   = Map.fromListWith (++) (classExports ++ [("Events",[]),("Null",[]),("Misc.",[])])
        methodMap  = Map.map sort (Map.fromListWith (++) (map exportDef decls))
        shortMap   = Map.map sort (Map.fromListWith (++) shortExports)
        exportMap  = Map.mapWithKey (addMethods methodMap shortMap) classMap
        eventEntry = case Map.lookup "Events" exportMap of
                       Just entry  -> [("Events",entry)]
                       Nothing     -> []
        miscEntry  = case Map.lookup "Misc." exportMap of
                       Just entry  -> [("Misc.",entry)]
                       Nothing     -> []
        nullEntry  = case Map.lookup "Null" exportMap of
                       Just entry  -> [("Null",entry)]
                       Nothing     -> []

        staticExps = map todef (nullEntry ++ eventEntry ++ miscEntry)
        classExps  = map todef (Map.toAscList (Map.delete "Null" (Map.delete "Misc." (Map.delete "Events" exportMap))))

    in  (concat staticExps
        ,concat classExps
        ,length (filter (not . null) classExps)
        )
  where
    addMethods methodMap shortMap className' _classDecls
      | null decls'' = []
      | otherwise    = [heading 2 className'] ++ decls''
      where
        decls'' =
          (case Map.lookup className' shortMap of
             Nothing    -> []
             Just decls' -> [heading 3 "Short methods"] ++ (commaSep decls')) ++
          (case Map.lookup className' methodMap of
             Nothing    -> []
             Just decls' -> (commaSep decls'))


    todef (_classname,decls')
      = decls'

    exportDef decl
      = (case classifyName (declName decl) of
           Name name     | isPrefixOf "expEVT_" name  -> "Events"
                         | isPrefixOf "Null_"   name  -> "Null"
                         | otherwise                  -> "Misc."
           Create name   -> haskellTypeName name
           Method name _ -> haskellTypeName name
        , [haskellDeclName (declName decl)])

    commaSep xs
      = map (exportComma++) xs

    heading i name
      = exportSpaces ++ "-- " ++ replicate i '*' ++ " " ++ name

{-----------------------------------------------------------------------------------------
   Short cut names  (unused)
-----------------------------------------------------------------------------------------}
{-
validShortNames :: [Decl] -> Set.Set String
validShortNames decls
  = Set.fromList
  $ map fst
  $ filter ((==1).snd)
  $ MultiSet.toOccurList
  $ MultiSet.fromList
  $ filter (any isUpper)
  $ filter (not.isBuiltin.headToUpper)
  $ filter (not.isClassName.headToUpper)
  $ filter (not.isPrefixOf "wx")
  $ filter ((>1).length)
  $ map shortName decls
  where
    headToUpper []      = []
    headToUpper (c:cs)  = toUpper c : cs
-}
{-
shortName :: Decl -> String
shortName decl
  = snd (shortNameEx decl)
-}
{-
shortNameEx :: Decl -> (String,String)
shortNameEx decl
  = case classifyName (declName decl) of
      Method cname (Normal name) -> (cname,haskellDeclName name)
      Method cname (Set name)    -> (cname,haskellDeclName  ("Set"++name))
      Method cname (Get name)    -> (cname,haskellDeclName  ("Get"++name))
      _other                     -> ("","")
-}
{-
shortDecl :: Set.Set String -> Decl -> [((ClassName,[String]), [String])]
shortDecl validShorts decl    | Set.member sname validShorts
  = [( (cname,[sname])
     , [haskellTypeSignature sname decl
       ,sname ++ " = " ++ haskellDeclName (declName decl)
       ,""]
     )
    ]
  where
    (cname,sname) = shortNameEx decl

shortDecl _validShorts _decl
  = []
-}

{-----------------------------------------------------------------------------------------
   Compile "xxx_Delete" methods to "objectDelete" to accomodate managed objects.
-----------------------------------------------------------------------------------------}
isDeleteMethod :: Decl -> Bool
isDeleteMethod decl
  = case (declRet decl, declArgs decl, classifyName (declName decl)) of
      (Void,[Arg [_] (Object selfName)],Method cname (Normal mname))
         -> (mname == "Delete" || mname=="SafeDelete")
            && (selfName == cname)
            && (cname `elem` objectClassNames || classIsManaged cname)
      _  -> False


{-----------------------------------------------------------------------------------------
   Make the "this" pointer the last argument
-----------------------------------------------------------------------------------------}
-- 2003-7-2: We disable this argument swapping and make it the first argument.
haskellThisArgument :: Decl -> (Maybe Arg,[Arg])
haskellThisArgument decl
  = (Nothing, declArgs decl)

haskellThisArgs :: Decl -> [(Bool,Arg)]
haskellThisArgs decl
  = case classifyName (declName decl) of
      Method name _  | not (null args) && (argType (head args) == Object name)
                     -> [(True,head args)] ++ [(False,arg) | arg <- tail args]
      _other         -> [(False,arg) | arg <- args]
  where
    args = declArgs decl

haskellSwapThis :: Decl -> [Arg]
haskellSwapThis decl
  = case haskellThisArgument decl of
      (Just this,args)  -> args ++ [this]
      (Nothing,args)    -> args

{-----------------------------------------------------------------------------------------
   Translate a declaration to a haskell marshalling wrapper
-----------------------------------------------------------------------------------------}
haskellDecl :: Decl -> String
haskellDecl decl | isDeleteMethod decl
  = haskellDeclName (declName decl) ++ nlStart
    ++ objDelete (classInfo (case classifyName (declName decl) of
                                Method cname _ -> cname
                                _              -> "wxObject"))


haskellDecl decl
  = methodName' ++ " " ++ haskellArgs (haskellSwapThis decl) ++ nlStart
    ++ haskellToCResult decl (declRet decl) (
           haskellToCArgsIO methodName' (haskellThisArgs decl)
        ++ foreignName (declName decl) ++ " " ++ haskellToCArgs decl (declArgs decl)
       )
  where
    methodName' = haskellDeclName (declName decl)

nl :: String
nl
  = "\n    "

nlStart :: String
nlStart
  = "\n  = "

pparens :: String -> String
pparens txt
  = "(" ++ txt ++ ")"


haskellArgs :: [Arg] -> String
haskellArgs args
  = concatMap (\arg -> haskellName (argName arg) ++ " ") args


haskellToCResult :: Decl -> Type -> String -> String
haskellToCResult decl tp call
  = unsafeIO $
    case tp of
      Fun _           -> traceWarning "function as result" decl $ call
      EventId         -> "withIntResult $" ++ nl ++ call
      Id              -> "withIntResult $" ++ nl ++ call
      Int _           -> "withIntResult $" ++ nl ++ call
      IntPtr          -> "withIntPtrResult $" ++ nl ++ call
      Bool            -> "withBoolResult $" ++ nl ++ call
      Char            -> "withCharResult $" ++ nl ++ call
      Object obj      -> withResult (classInfo obj)  ++ " $" ++ nl ++ call
      String _        -> "withWStringResult $ \\buffer -> " ++ nl ++ call ++ " buffer"    -- always last argument!
      ByteString Lazy -> "withLazyByteStringResult $ \\buffer -> " ++ nl ++ call ++ " buffer"    -- always last argument!
      ByteString _    -> "withByteStringResult $ \\buffer -> " ++ nl ++ call ++ " buffer"    -- always last argument!
      Point CDouble   -> "withPointDoubleResult $ \\px py -> " ++ nl ++ call ++ " px py"       -- always last argument!
      Point _         -> "withPointResult $ \\px py -> " ++ nl ++ call ++ " px py"       -- always last argument!
      Vector CDouble  -> "withVectorDoubleResult $ \\pdx pdy -> " ++ nl ++ call ++ " pdx pdy"       -- always last argument!
      Vector _        -> "withVectorResult $ \\pdx pdy -> " ++ nl ++ call ++ " pdx pdy"       -- always last argument!
      Size CDouble    -> "withSizeDoubleResult $ \\pw ph -> " ++ nl ++ call ++ " pw ph"       -- always last argument!
      Size _          -> "withSizeResult $ \\pw ph -> " ++ nl ++ call ++ " pw ph"       -- always last argument!
      Rect CDouble    -> "withRectDoubleResult $ \\px py pw ph -> " ++ nl ++ call ++ "px py pw ph"       -- always last argument!
      Rect _          -> "withRectResult $ \\px py pw ph -> " ++ nl ++ call ++ "px py pw ph"       -- always last argument!

      RefObject name  -> case withRef (classInfo name) of
                           ""     -> errorMsgDecl decl "illegal reference object"
                           action -> action ++ " $ \\pref -> " ++ nl ++ call ++ " pref"  -- always last argument!
      ArrayInt _      -> "withArrayIntResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      ArrayIntPtr _   -> "withArrayIntPtrResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      ArrayString _   -> "withArrayWStringResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      ArrayObject _ _ -> "withArrayObjectResult $ \\arr -> " ++ nl ++ call ++ " arr" -- always last
      _other          -> call
  where
    unsafeIO body
      = case tp of
          EventId  -> "unsafePerformIO $" ++ nl ++ body
          Id       -> "unsafePerformIO $" ++ nl ++ body
          _other   | isPrefixOf "Null_" (declName decl)  -> "unsafePerformIO $" ++ nl ++ body
                   | otherwise -> body


haskellToCArgsIO :: String -> [(Bool, Arg)] -> String
haskellToCArgsIO methodName' args
  = concatMap (\(isSelf,arg) -> haskellToCArgIO methodName' isSelf arg) args


haskellToCArgIO :: String -> Bool -> Arg -> String
haskellToCArgIO methodName' isSelf arg
  = case argType arg of
      String _    -> "withCWString " ++ haskellName (argName arg)
                      ++ " $ \\" ++ haskellCStringName (argName arg) ++ " -> " ++ nl
      ByteString Lazy -> "withArray (LB.unpack " ++ haskellName (argName arg) ++ ") $ \\"
                      ++ haskellByteStringName (argName arg)
                      ++ " -> " ++ nl
      ByteString _ -> "B.useAsCStringLen " ++ haskellName (argName arg) ++ " $ \\"
                      ++ "(" ++ haskellByteStringName (argName arg) ++ ", " ++ haskellByteStringLenName (argName arg) ++ ") "
                      ++ " -> " ++ nl
      ArrayString _
                  -> "withArrayWString " ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellArrayLenName (argName arg) ++ " " ++ haskellArrayName (argName arg)
                     ++ " -> " ++ nl
      ArrayObject _tp _
                  -> "withArrayObject " ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellArrayLenName (argName arg) ++ " " ++ haskellArrayName (argName arg)
                     ++ " -> " ++ nl
      ArrayInt _
                  -> "withArrayInt " ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellArrayLenName (argName arg) ++ " " ++ haskellArrayName (argName arg)
                     ++ " -> " ++ nl
      ArrayIntPtr _
                  -> "withArrayIntPtr " ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellArrayLenName (argName arg) ++ " " ++ haskellArrayName (argName arg)
                     ++ " -> " ++ nl
      Object obj  -> (if isSelf then withSelf (classInfo obj) ("\"" ++ methodName' ++ "\"")
                                else withPtr (classInfo obj)) ++ " "
                     ++ haskellName (argName arg)
                     ++ " $ \\" ++ haskellCObjectName (argName arg) ++ " -> " ++ nl
      _other      -> ""


haskellToCArgs :: Decl -> [Arg] -> String
haskellToCArgs decl args
  = concatMap (\arg -> haskellToCArg decl arg ++ "  ") args


haskellToCArg :: Decl -> Arg -> String
haskellToCArg decl arg
  = case argType arg of
      RefObject _     -> traceError "reference object as argument" decl $ name
      EventId         -> traceError "event id as argument" decl $ name
      Id              -> traceError "id as argument" decl $ name
      Int _           -> pparens ("toCInt " ++ name)
      IntPtr          -> pparens ("toCIntPtr " ++ name)
      Char            -> pparens ("toCWchar " ++ name)
      Bool            -> pparens ("toCBool " ++ name)
      Fun _           -> pparens ("toCFunPtr " ++ name)

      String _        -> haskellCStringName (argName arg)
      ByteString Lazy -> haskellByteStringName name ++ " (fromIntegral $ LB.length " ++ haskellName name ++ ")"
      ByteString _    -> haskellByteStringName name ++ " " ++ haskellByteStringLenName name
      Object     _    -> haskellCObjectName (argName arg)
      Point CDouble   -> pparens ("toCDoublePointX " ++ name) ++ " " ++ pparens( "toCDoublePointY " ++ name)
      Point _         -> pparens ("toCIntPointX " ++ name) ++ " " ++ pparens( "toCIntPointY " ++ name)
      Vector CDouble  -> pparens ("toCDoubleVectorX " ++ name) ++ " " ++ pparens( "toCDoubleVectorY " ++ name)
      Vector _        -> pparens ("toCIntVectorX " ++ name) ++ " " ++ pparens( "toCIntVectorY " ++ name)
      Size CDouble    -> pparens ("toCDoubleSizeW " ++ name) ++ " " ++ pparens( "toCDoubleSizeH " ++ name)
      Size _          -> pparens ("toCIntSizeW " ++ name) ++ " " ++ pparens( "toCIntSizeH " ++ name)
      Rect CDouble    -> pparens ("toCDoubleRectX " ++ name) ++ " " ++ pparens( "toCDoubleRectY " ++ name)
                         ++ pparens ("toCDoubleRectW " ++ name) ++ " " ++ pparens( "toCDoubleRectH " ++ name)
      Rect _          -> pparens ("toCIntRectX " ++ name) ++ " " ++ pparens( "toCIntRectY " ++ name)
                         ++ pparens ("toCIntRectW " ++ name) ++ " " ++ pparens( "toCIntRectH " ++ name)
      ColorRGB _      ->    pparens ("colorRed " ++ name) ++ " "
                         ++ pparens ("colorGreen " ++ name) ++ " "
                         ++ pparens ("colorBlue " ++ name)

      ArrayString _   -> haskellArrayLenName name ++ " " ++ haskellArrayName name
      ArrayObject _ _ -> haskellArrayLenName name ++ " " ++ haskellArrayName name
      ArrayInt _      -> haskellArrayLenName name ++ " " ++ haskellArrayName name
      ArrayIntPtr _   -> haskellArrayLenName name ++ " " ++ haskellArrayName name

      _other -> name
  where
    name = haskellName (argName arg)

haskellCStringName :: String -> String
haskellCStringName name
  = "cstr_" ++ haskellName name

haskellByteStringName :: String -> String
haskellByteStringName name
  = "bs_" ++ haskellName name

haskellByteStringLenName :: String -> String
haskellByteStringLenName name
  = "bslen_" ++ haskellName name

haskellArrayName :: String -> String
haskellArrayName name
  = "carr_" ++ haskellName name

haskellArrayLenName :: String -> String
haskellArrayLenName name
  = "carrlen_" ++ haskellName name

haskellCObjectName :: String -> String
haskellCObjectName name
  = "cobj_" ++ haskellName name

{-----------------------------------------------------------------------------------------
   Translate a declaration to a haskell type declaration
-----------------------------------------------------------------------------------------}
-- | Generate a full haskell type declarations
haskellTypeDecl :: Decl -> String
haskellTypeDecl decl
  = haskellHaddockComment decl ++ "\n" ++
    haskellTypeSignature (haskellDeclName (declName decl)) decl

-- | Generate a haddock comment
haskellHaddockComment :: Decl -> String
haskellHaddockComment decl
  | null (declComment decl) =  "-- | usage: (@" ++ callExpr ++ "@)."
  | otherwise               =  "{- | " ++ declComment decl ++ " -}"
  where
    callExpr = case haskellThisArgument decl of
                 (Just this,args) -> haskellArgName (argName this) ++ " # " ++ haskellDeclName (declName decl) ++ callArgs args
                 (Nothing,args)   -> haskellDeclName (declName decl) ++ callArgs args
    callArgs args
             = concatMap (\arg -> " " ++ haskellArgName (argName arg)) args

-- | Generate a haskell type signature
haskellTypeSignature :: String -> Decl -> String
haskellTypeSignature name decl
  = haskellRetType decl $
    name ++ " :: "  ++ haskellTypeArgs decl (haskellSwapThis decl)


haskellTypeArgs :: Decl -> [Arg] -> String
haskellTypeArgs decl args
  = concatMap (\(i,arg) -> haskellTypeArg decl i arg ++ " -> ") (zip [1..] args)


haskellRetType :: Decl -> [Char] -> String
haskellRetType decl typedecl
  = case declRet decl of
      EventId   -> "{-# NOINLINE " ++ haskellDeclName (declName decl) ++ " #-}\n" ++ typedecl ++ " EventId"
      Id      -> "{-# NOINLINE " ++ haskellDeclName (declName decl) ++ " #-}\n" ++ typedecl ++ " Int"
      tp        | isPrefixOf "Null_" (declName decl)
                -> typedecl ++ haskellType 0 tp
                | otherwise
                -> typedecl ++ " IO " ++ haskellTypePar 0 tp



-- type def. for clarity
haskellTypeArg :: a -> Int -> Arg -> String
haskellTypeArg _decl _i (Arg ["id"] (Int _))     = "Id"
haskellTypeArg _decl _i (Arg ["_id"] (Int _))    = "Id"
haskellTypeArg _decl _i (Arg ["_stl"] (Int _))   = "Style"
haskellTypeArg _decl i  arg
  = haskellType i (argType arg)


haskellTypePar :: Int -> Type -> String
haskellTypePar i tp
  = parenType (haskellType i) tp


haskellType :: Int -> Type -> String
haskellType i tp
  = case tp of
      Bool     -> "Bool"
      Int _    -> "Int"
      IntPtr   -> "IntPtr"
      Int64    -> "Int64"
      Word     -> "Word"
      Word8    -> "Word8"
      Word32   -> "Word32"
      Word64   -> "Word64"
      Void     -> "()"
      Char     -> "Char"
      Double   -> "Double"
      Float    -> "Float"
      Ptr Void -> "Ptr " ++ typeVar i
      Ptr t    -> "Ptr " ++ foreignTypePar i t
      -- special
      Vector CDouble     -> "(Vector2 Double)"
      Vector _           -> "Vector"
      Point CDouble      -> "(Point2 Double)"
      Point _            -> "Point"
      Size CDouble       -> "(Size2D Double)"
      Size _             -> "Size"
      ColorRGB _         -> "Color"
      String _           -> "String"
      ByteString Lazy    -> "LB.ByteString"
      ByteString _       -> "B.ByteString"
      ArrayString _      -> "[String]"
      ArrayInt _         -> "[Int]"
      ArrayIntPtr _      -> "[IntPtr]"
      ArrayObject name _ -> "[" ++ haskellTypeName name ++ typeVar i ++ "]"
      Rect CDouble       -> "(Rect2D Double)"
      Rect _             -> "Rect"
      Fun f              -> "FunPtr " ++ pparens f
      RefObject name     -> classTypeName (classInfo name) (typeVar i) -- haskellTypeName name ++ typeVar i
      Object name        -> classTypeName (classInfo name) (typeVar i) -- haskellTypeName name ++ typeVar i
      _other             -> error ("Non exaustive pattern: CompileClasses.haskellType: " ++ show tp)

{-----------------------------------------------------------------------------------------
   Translate a declaration to a foreign import declaration
-----------------------------------------------------------------------------------------}
foreignDecl :: Decl -> String
foreignDecl decl | isDeleteMethod decl
  = ""

foreignDecl decl
  = "foreign import ccall \"" ++ declName decl ++ "\" "
      ++ foreignName (declName decl) ++ " :: "
      ++ foreignArgs decl (declArgs decl) ++ foreignResultType (declRet decl)


foreignName :: String -> String
foreignName name
  | isPrefixOf "wx" name  && elem '_' name  = name
  | otherwise                               = "wx_" ++ name

foreignArgs :: Decl -> [Arg] -> String
foreignArgs decl args
  = concatMap (\(i,arg) -> foreignArg decl i arg ++ " -> ") (zip [1..] args)


foreignArg :: Decl -> Int -> Arg -> String
foreignArg decl i arg
  = case argType arg of
      RefObject name -> traceError "RefObject in argument" decl $ foreignType i (RefObject name)
      Void           -> traceError "void type in argument" decl $ foreignType i Void
      tp             -> foreignType i tp


foreignResultType :: Type -> String
foreignResultType tp
  = case tp of
      ArrayInt _         -> "Ptr CInt -> IO CInt"
      ArrayIntPtr _      -> "Ptr CIntPtr -> IO CInt"
      ArrayString _      -> "Ptr (Ptr CWchar) -> IO CInt"
      ArrayObject name _ -> "Ptr " ++ foreignTypePar 0 (Object name) ++ " -> IO CInt"
      String _           -> "Ptr CWchar -> IO CInt"
      ByteString _       -> "Ptr CChar -> IO CInt"
      Point CDouble      -> "Ptr CDouble -> Ptr CDouble -> IO ()"
      Point _            -> "Ptr CInt -> Ptr CInt -> IO ()"
      Vector CDouble     -> "Ptr Double -> Ptr Double -> IO ()"
      Vector _           -> "Ptr CInt -> Ptr CInt -> IO ()"
      Size CDouble       -> "Ptr CDouble -> Ptr CDouble -> IO ()"
      Size _             -> "Ptr CInt -> Ptr CInt -> IO ()"
      Rect CDouble       -> "Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()"
      Rect _             -> "Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()"
      RefObject _name    -> foreignType 0 tp ++ " -> IO ()"
      EventId            -> "IO CInt"
      Id                 -> "IO CInt"
      _other             -> "IO " ++ foreignTypePar 0 tp


foreignTypePar :: Int -> Type -> String
foreignTypePar i tp
  = parenType (foreignType i) tp


foreignType :: Int -> Type -> [Char]
foreignType i tp
  = case tp of
      Bool   -> "CBool"
      Int _  -> "CInt"
      Int64  -> "Int64"
      IntPtr -> "CIntPtr"
      Word   -> "Word"
      Word8  -> "Word8"
      Word32 -> "Word32"
      Word64 -> "Word64"
      Void   -> "()"
      Char   -> "CWchar"
      Double -> "Double"
      Float  -> "Float"
      Ptr Void  -> "Ptr " ++ typeVar i
      Ptr t     -> "Ptr " ++ foreignTypePar i t
      -- special
      String _           -> "CWString"
      ByteString Lazy    -> "Ptr Word8 -> Int"
      ByteString _       -> "Ptr CChar -> Int"
      Point CDouble      -> "CDouble -> CDouble"
      Point _            -> "CInt -> CInt"
      Vector CDouble     -> "CDouble -> CDouble"
      Vector _           -> "CInt -> CInt"
      Size CDouble       -> "CDouble -> CDouble"
      Size _             -> "CInt -> CInt"
      ColorRGB _         -> "Word8 -> Word8 -> Word8"
      Rect CDouble       -> "CDouble -> CDouble -> CDouble -> CDouble"
      Rect _             -> "CInt -> CInt -> CInt -> CInt"
      Fun f              -> "Ptr " ++ pparens f
      ArrayObject name _ -> "CInt -> Ptr " ++ foreignTypePar i (Object name)
      ArrayString _      -> "CInt -> Ptr (Ptr CWchar)"
      ArrayInt _         -> "CInt -> Ptr CInt"
      ArrayIntPtr _      -> "CInt -> Ptr CIntPtr"
      RefObject name     -> "Ptr (T" ++ haskellTypeName name ++ typeVar i ++ ")"
      Object name        -> "Ptr (T" ++ haskellTypeName name ++ typeVar i ++ ")"
      _                  -> error $ "CompileClasses.foreignType: unexpected type: " ++ show tp


parenType :: (Type -> String) -> Type -> String
parenType f tp
  = parenFun tp (f tp)
  where
    parenFun tp'
      = case tp' of
          Ptr _       -> pparens
          Object _    -> pparens
          RefObject _ -> pparens
          _other      -> id


typeVar :: Int -> String
typeVar i = " " ++ typeVars !! i

typeVars :: [String]
typeVars  = "()" : [[toEnum (fromEnum 'a' + x)] | x <- [0..]]
