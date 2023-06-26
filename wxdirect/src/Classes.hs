-----------------------------------------------------------------------------------------
{-| Module      :  Classes
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Defines most of the classes in wxWidgets.
-}
-----------------------------------------------------------------------------------------
module Classes( isClassName, isBuiltin, haskellClassDefs
              , objectClassNames, classNames
              , classExtends
              , getWxcDir, setWxcDir
              -- * Class info
              , ClassInfo(..)
              , classInfo
              , classIsManaged
              , findManaged
              , managedClasses
              ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import System.FilePath ((</>))
import Text.Parsec.Prim hiding ( try )
import HaskellNames( haskellTypeName, isBuiltin )
import Types

-- to parse a class hierarchy
import Text.ParserCombinators.Parsec
import ParseC( readHeaderFile )

-- unsafe hack :-(
import System.IO.Unsafe( unsafePerformIO )
import Data.IORef



-- urk, ugly hack to make "classes" function pure.
{-# NOINLINE wxcdir #-}
wxcdir :: IORef String
wxcdir
  = unsafePerformIO $
    do newIORef ("../wxc")

getWxcDir :: IO String
getWxcDir
  = readIORef wxcdir

setWxcDir :: String -> IO ()
setWxcDir dir
  = writeIORef wxcdir dir

{-----------------------------------------------------------------------------------------

-----------------------------------------------------------------------------------------}
{-
ignoreClasses :: Set.Set String
ignoreClasses
  = Set.fromList ["wxFile", "wxDir", "wxString", "wxManagedPtr"]
-}

classes :: [Class]
classes
  = unsafePerformIO $
    do 
       -- urk, ugly hack.
       wxcdir' <- getWxcDir
       cs <- parseClassDefs (wxcdir' </> "wxc/wxc.h")
       return cs


mergeClasses :: [Class] -> [Class] -> [Class]
mergeClasses xs ys
  = foldr (\c cs -> mergeClass c cs) xs ys


mergeClass :: Class -> [Class] -> [Class]
mergeClass cls []   = [cls]
mergeClass cls1@(Class name1 subs1)  (cls2@(Class name2 subs2) : cs)
  | name1 == name2  = Class name2 (mergeClasses subs1 subs2) : cs
  | otherwise       = cls2:mergeClass cls1 cs


{-----------------------------------------------------------------------------------------
  Managed classes
-----------------------------------------------------------------------------------------}
data ClassInfo = ClassInfo{ classWxName   :: String
                          , withSelf      :: String -> String
                          , withPtr       :: String
                          , withResult    :: String
                          , withRef       :: String
                          , objDelete     :: String
                          , classTypeName :: String -> String
                          }
                  
classIsManaged :: String -> Bool
classIsManaged name
  = case findManaged name of
      Just _  -> True
      Nothing -> False

classInfo :: String -> ClassInfo
classInfo name
  = case findManaged name of
      Just info -> info
      Nothing   -> standardInfo name

findManaged :: String -> Maybe ClassInfo
findManaged name
  = find managedClasses
  where
    find [] = Nothing
    find (info:rest) | classWxName info == name = Just info
                     | otherwise                = find rest


standardInfo :: String -> ClassInfo
standardInfo name
  = ClassInfo name (\methodName -> "withObjectRef " ++ methodName) "withObjectPtr" "withObjectResult" 
                    "" "objectDelete" (\typevar -> haskellTypeName name ++ " " ++ typevar)

managedClasses :: [ClassInfo]
managedClasses 
  = -- standard reference objects with a distinguished static object. (i.e. wxNullBitmap)
    map standardNull 
    ["Bitmap"
    ,"Cursor"
    ,"Icon"
    ,"Font"
    ,"Pen"
    ,"Brush"
    ] ++

    -- standard reference objects
    map standardRef
    ["Image"
    ,"FontData"
    ,"ListItem"
    ,"PrintData"
    ,"PrintDialogData"
    ,"PageSetupDialogData"] ++

    -- standard reference object, but not a subclass of wxObject
    [ ClassInfo "wxDateTime" (affix "withObjectRef") "withObjectPtr" "withManagedDateTimeResult" 
                    "withRefDateTime" "dateTimeDelete" (affix "DateTime")
    , ClassInfo "wxGridCellCoordsArray" (affix "withObjectRef") "withObjectPtr" 
                    "withManagedGridCellCoordsArrayResult"   
                    "withRefGridCellCoordsArray" "gridCellCoordsArrayDelete" (affix "GridCellCoordsArray")
    ] ++


    -- managed objects (that are not passed by reference)
    map standard
    ["Sound"] ++

    -- translated directly to a Haskell datatype
    [ ClassInfo "wxColour" (affix "withColourRef") "withColourPtr" "withManagedColourResult"
                     "withRefColour" "const (return ())" (const "Color")
    , ClassInfo "wxString" (affix "withStringRef") "withStringPtr" "withManagedStringResult"
                     "withRefString" "const (return ())" (const "String")
    , ClassInfo "wxPoint" (affix "withPointRef") "withPointPtr" "withWxPointResult"
                     "withRefPoint" "const (return ())" (const "Point")
    , ClassInfo "wxSize" (affix "withSizeRef") "withSizePtr" "withWxSizeResult"
                     "withRefSize" "const (return ())" (const "Size")
    , ClassInfo "wxRect" (affix "withWxRectRef") "withWxRectPtr" "withWxRectResult"
                     "withRefRect" "const (return ())" (const "Rect")
    , ClassInfo "wxTreeItemId" (affix "withTreeItemIdRef") "withTreeItemIdPtr" "withManagedTreeItemIdResult"
                     "withRefTreeItemId" "const (return ())" (const "TreeItem")
    ]


  where
    standardNull name
      = (standardRef name){ withResult = "withManaged" ++ name ++ "Result" }

    standardRef name
      = (standard name){ withRef = "withRef" ++ name }

    standard name
      = ClassInfo ("wx" ++ name)  (affix "withObjectRef") "withObjectPtr" "withManagedObjectResult" 
                    "" "objectDelete" (affix name)
        
    affix name arg
      = name ++ " " ++ arg

{-----------------------------------------------------------------------------------------
   Classes
-----------------------------------------------------------------------------------------}
data Class
  = Class String [Class]
  deriving Eq

instance Show Class where
  showsPrec _ c
    = showString (showClass 0 c)


{-
showClasses :: [Class] -> String
showClasses cs
  = unlines (map (showClass 0) cs)
-}

showClass :: Int -> Class -> [Char]
showClass indent (Class name subs)
  = (replicate indent '\t' ++ name ++ concatMap ("\n"++) (map (showClass (indent+1)) subs))

isClassName :: String -> Bool
isClassName s
  = Set.member s classNames

objectClassNames :: [String]
objectClassNames
  = case filter isObject classes of
      [classObject] -> flatten classObject
      _             -> []
  where
    flatten (Class name derived)
      = name : concatMap flatten derived

    isObject (Class name _derived)
      = (name == "wxObject")


classNames :: Set.Set String
classNames
  = Set.unions (map flatten classes)
  where
    flatten (Class name derived)
      = Set.insert name (Set.unions (map flatten derived))

classExtends :: Map.Map String String
classExtends
  = Map.unions (map (flatten "") classes)
  where
    flatten parent (Class name derived)
      = Map.insert name parent (Map.unions (map (flatten name) derived))


{-
sortClasses :: [Class] -> [Class]
sortClasses cs
  = map sortExtends (sortBy cmp cs)
  where
    cmp (Class name1 _) (Class name2 _) = compare name1 name2

    sortExtends (Class name extends)
      = Class name (sortClasses extends)
-}

haskellClassDefs :: ([(String,[String])],[String])     -- exported, definitions
haskellClassDefs
  = unzip (concatMap (haskellClass []) classes)


haskellClass :: [[Char]] -> Class -> [(([Char], [[Char]]), [Char])]
haskellClass parents (Class name derived)
--  | isBuiltin name = []   -- handled as a basic type
--  | otherwise
    = ( (tname,[tname,inheritName tname,className tname])
      , (
          ("-- | Pointer to an object of type '" ++ tname ++ "'" ++
          (if null parents then "" else ", derived from '" ++ head parents ++ "'") ++
          ".\n" ++
          "type " ++ tname ++ " a  = " ++  inheritance)
        ) ++ "\n" ++
        "-- | Inheritance type of the " ++ tname ++ " class.\n" ++
        "type " ++ inheritName tname ++ " a  = " ++ inheritanceType ++ "\n" ++
        "-- | Abstract type of the " ++ tname ++ " class.\n" ++
        "data " ++ className tname ++ " a  = " ++ className tname ++ "\n"
      )
     : concatMap (haskellClass (tname:parents)) derived
  where
    tname         = haskellTypeName name
    className s   = "C" ++ haskellTypeName s
    inheritName s = "T" ++ haskellTypeName s

{-
    explicitInheritance
      = foldl extend (className tname ++ " a") parents
      where
        extend child parent
          = "C"++parent ++ " " ++ pparens child
-}
    inheritanceType
      = (if null parents then id else (\tp -> inheritName (head parents) ++ " " ++ pparens tp))
         (className tname ++ " a")

    inheritance
      = (if null parents then "Object " else (haskellTypeName (head parents) ++ " "))
        ++ pparens (className tname ++ " a")


pparens :: [Char] -> [Char]
pparens txt
  = "(" ++ txt ++ ")"


{-----------------------------------------------------------------------------------------
   Read a class hierarchy from file.
   The format consists of all classes on a line,
   with subclassing expressed by putting tabs in front of the class.
   see: http://www.wxwindows.org/classhierarchy.txt
-----------------------------------------------------------------------------------------}
{-
parseClassHierarchy :: FilePath -> IO [Class]
parseClassHierarchy fname
  = do result <- parseFromFile parseClasses (if null fname then "classhierarchy.txt" else fname)
       case result of
         Left err  -> do putStrLn ("parse error in class hierarchy: " ++ show err)
                         return []
         Right cs -> return cs
    `catch` \(SomeException err) ->
     do putStrLn ("exception while parsing: " ++ fname)
        print err
        return []

parseClasses :: Parser [Class]
parseClasses
  = do cs <- pclasses 0
       eof
       return cs

pclasses :: Int -> Parser [Class]
pclasses indent
  = do css <- many (pclass indent)
       return (concat css)
  <?> "classes"

pclass :: Int -> Parser [Class]
pclass indent
  = do _    <- try (count indent pindent)
       name <- pclassName
       _    <- whiteSpace
       mkClass
            <- (do _ <- char '\n'
                   return (\subs -> filterClass (Class name subs))
                <|>
                do name2 <- try $
                             do _     <- char '='
                                _     <- whiteSpace
                                name2 <- pclassName
                                _     <- whiteSpace
                                _     <- char '\n'
                                return name2
                   return (\subs -> filterClass (Class name2 subs))
                <|>
                do _ <- skipToEndOfLine
                   return (\_subs -> []))
       subs <- pclasses (indent+1)
       return (mkClass subs)
  <|>
    do _ <- char '\n'
       return []
  <?> "class"
-}

{-
filterClass :: Class -> [Class]
filterClass (Class name subs)
  | not (Set.member name ignoreClasses) = [Class name subs]
filterClass cls
  = []
-}

{-
pindent :: Parser ()
pindent
  = (char '\t'     >> return ()) <|> 
    (count 8 space >> return ())
  <?> ""
-}

{-
pclassName :: Parser [Char]
pclassName
  = many1 alphaNum
  <?> "class name"
-}
{-
skipToEndOfLine :: Parser Char
skipToEndOfLine
  = do _ <- many (noneOf "\n")
       char '\n'
-}

whiteSpace :: Parser [Char]
whiteSpace
  = many (oneOf " \t")

{-----------------------------------------------------------------------------------------
  parse class hierarchy from class definitions in a C header files:
  TClassDef(tp)
  TClassDefExtend(tp,parent)
-----------------------------------------------------------------------------------------}
parseClassDefs :: FilePath -> IO [Class]
parseClassDefs fname
  = do putStrLn "reading class definitions:"
       contents <- readHeaderFile fname
       let defs    = filter (not . null . fst) (map parseClassDef contents)
           extends = Map.fromList defs
           extend name
                   = complete (Class name [])
                   where
                     complete cls@(Class cname _ext)
                       = case Map.lookup cname extends of
                           Just ""     -> cls
                           Just parent -> complete (Class parent [cls])
                           Nothing     -> trace ("warning: undefined base class " ++ show cname ++ " in definition of " ++ show name) $
                                          cls
           clss    = map (extend . fst) defs
       return (foldr (\c cs -> mergeClass c cs) [] clss)

parseClassDef :: String -> (String,String)
parseClassDef line
  = case parse pdef "" line of
      Left _err -> ("","")
      Right r   -> r

pdef :: Parser (String,String)
pdef
  = do _   <- reserved "TClassDefExtend"
       _   <- psymbol "("
       tp  <- identifier
       _   <- psymbol ","
       ext <- identifier
       _   <- psymbol ")"
       return (tp,ext)
  <|>
    do _  <- reserved "TClassDef"
       tp <- parens identifier
       return (tp,"")


parens :: Parser b -> Parser b
parens p
  = do { _ <- psymbol "("; x <- p; _ <- psymbol ")"; return x }

psymbol :: String -> Parser String
psymbol s
  = lexeme (string s)

reserved :: String -> Parser String
reserved s
  = lexeme (try (string s))

identifier :: Parser String
identifier
  = lexeme (many1 alphaNum)

lexeme :: Parser b -> Parser b
lexeme p
  = do{ x <- p
      ; _ <- whiteSpace
      ; return x
      }
