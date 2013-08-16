-----------------------------------------------------------------------------------------
{-| Module      :  CompileHeader
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Module that compiles typed C definitions from untyped ones.
-}
-----------------------------------------------------------------------------------------
module CompileHeader( compileHeader ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.List( isPrefixOf, sort, sortBy, intersperse )

import Types
import HaskellNames
import Classes( classNames, classExtends )
import ParseC( parseC )
import DeriveTypes( deriveTypesAll, classifyName, Name(..) )
import IOExtra

{-----------------------------------------------------------------------------------------
  Compile
-----------------------------------------------------------------------------------------}
compileHeader :: Bool -> FilePath -> [FilePath] -> IO ()
compileHeader showIgnore outputFile inputFiles
  = do declss  <- mapM parseC inputFiles
       -- time <- getCurrentTime
       let decls        = deriveTypesAll showIgnore (sortBy cmpDecl (concat declss))

           typeDecls    = cTypeDecls decls

           methodCount  = length decls

       let output  = unlines (["#ifndef WXC_GLUE_H"
                              ,"#define WXC_GLUE_H"]
                              ++ typeDecls ++
                              [""
                              ,"#endif /* WXC_GLUE_H */"
                              ,""]
                              )

       putStrLn ("generating: " ++ outputFile)
       writeFileLazy outputFile output
       putStrLn ("generated " ++ show methodCount ++ " declarations.")
       putStrLn ("ok.\n")


cmpDecl :: Decl -> Decl -> Ordering
cmpDecl decl1 decl2
  = compare (haskellDeclName (declName decl1)) (haskellDeclName (declName decl2))

{-
exportComma :: String
exportComma  = exportSpaces ++ ","
-}

{-
exportSpaces :: String
exportSpaces = "     "
-}

{-----------------------------------------------------------------------------------------
   Translate declarations to a c type declarations
-----------------------------------------------------------------------------------------}
cTypeDecls :: [Decl] -> [String]
cTypeDecls decls
  = let classMap   = Map.fromList (map (\name -> (name,[])) (Set.elems classNames ++ ["Events","Null","Misc."]))
        methodMap  = Map.map (map snd) (Map.map sort (Map.fromListWith (++) (map typeDef decls)))
        tdeclMap   = Map.mapWithKey (addMethods methodMap) classMap
        eventEntry = case Map.lookup "Events" tdeclMap of
                       Just entry  -> [("Events",entry)]
                       Nothing     -> []
        miscEntry  = case Map.lookup "Misc." tdeclMap of
                       Just entry  -> [("Misc.",entry)]
                       Nothing     -> []
        nullEntry  = case Map.lookup "Null" tdeclMap of
                       Just entry  -> [("Null",entry)]
                       Nothing     -> []

    in  (concatMap toDecls (nullEntry ++ eventEntry ++ miscEntry)
        ++
         concatMap toDecls (Map.toAscList (Map.delete "Null" (Map.delete "Misc." (Map.delete "Events" tdeclMap))))
        )
  where
    addMethods methodMap className' classDecls
      = heading className' ++
        (case Map.lookup className' classExtends of
          Nothing  -> []
          Just ""  -> ["TClassDef(" ++ className' ++ ")"]
          Just ext -> ["TClassDefExtend(" ++ className' ++ "," ++ ext ++ ")"]
        )
        ++ classDecls ++
        (case Map.lookup className' methodMap of
           Nothing   -> []
           Just dcls -> dcls)


    toDecls (_classname, dcls)
      = dcls

    typeDef decl
      = (case classifyName (declName decl) of
           Name name     | isPrefixOf "expEVT_" name  -> "Events"
                         | isPrefixOf "Null_"   name  -> "Null"
                         | otherwise                  -> "Misc."
           Create name   -> name
           Method name _ -> name
        , [(declName decl, cTypeDecl decl)])


    heading msg
      = ["","/* " ++ msg ++ " */"]


{-----------------------------------------------------------------------------------------
   Translate a declaration to a c type declaration
-----------------------------------------------------------------------------------------}
-- | Generate a full c type declaration
cTypeDecl :: Decl -> String
cTypeDecl decl
  = cTypeSignature decl

-- | Generate a haskell type signature
cTypeSignature :: Decl -> String
cTypeSignature decl
  = fill 10 (cRetType decl (declRet decl)) ++
    " _stdcall " ++ declName decl ++
    "( "  ++ concat (intersperse ", " (cTypeArgs decl (declArgs decl) ++ cOutArg (declRet decl))) ++ " );"


fill :: Int -> String -> String
fill n s
  | length s >= n  = s
  | otherwise      = s ++ replicate (n - length s) ' '


cTypeArgs :: Decl -> [Arg] -> [String]
cTypeArgs _    []         = []
cTypeArgs decl (arg:args)
  = cTypeArg decl getClassName arg : map (cTypeArg decl "") args
  where
    getClassName = case classifyName (declName decl) of
                     Method cname _  -> cname
                     _otherwise      -> ""

cRetType :: Decl -> Type -> String
cRetType decl tp
  = case tp of
      -- out
      String _        -> "TStringLen"
      ArrayString _   -> "TArrayLen"
      ArrayObject _ _ -> "TArrayLen"
      Vector _        -> "void"
      Point _         -> "void"
      Size _          -> "void"
      Rect _          -> "void"
      RefObject _     -> "void"
      -- typedefs
      EventId     -> "int"
      -- basic
      Bool        -> "TBool"
      Char        -> "TChar"
      Int CLong   -> "long"
      Int TimeT   -> "time_t"
      Int SizeT   -> "size_t"
      Int _       -> "int"
      Void        -> "void"
      Double      -> "double"
      Float       -> "float"
      Ptr Void    -> "void*"
      Ptr t       -> cRetType decl t ++ "*"
      Object name -> "TClass(" ++  name ++ ")"
      _other      -> traceError ("unknown return type (" ++ show tp ++ ")") decl $
                       "void"

cOutArg :: Type -> [String]
cOutArg tp
  = case tp of
      Vector ctp           -> ["TVectorOut" ++ ctypeSpec CInt ctp    ++ "(_vx,_vy)"]
      Point ctp            -> ["TPointOut" ++ ctypeSpec CInt ctp    ++ "(_x,_y)"]
      Size ctp             -> ["TSizeOut" ++ ctypeSpec CInt ctp    ++ "(_w,_h)"]
      String ctp           -> ["TStringOut" ++ ctypeSpec CChar ctp ++ " _buf"]
      Rect ctp             -> ["TRectOut" ++ ctypeSpec CInt ctp    ++ "(_x,_y,_w,_h)" ]
      RefObject name       -> ["TClassRef(" ++  name ++ ") _ref"]
      ArrayString ctp      -> ["TArrayString" ++ ctypeSpec CChar ctp ++ " _strs"]
      ArrayObject name ctp -> ["TArrayObject" ++ ctypeSpec CObject ctp ++ "(" ++ name ++ ") _objs"]
      _other               -> []


-- type def. for clarity
cTypeArg :: Decl -> String -> Arg -> String
cTypeArg decl className' arg
  = case argType arg of
      -- basic
      Bool      -> "TBool " ++ argName arg
      Char      -> "TChar " ++ argName arg
      Int CLong -> "long " ++ argName arg
      Int TimeT -> "time_t " ++ argName arg
      Int SizeT -> "size_t " ++ argName arg
      Int _     -> "int " ++ argName arg
      Void      -> "void " ++ argName arg
      Double    -> "double " ++ argName arg
      Float     -> "float " ++ argName arg
      Ptr Void  -> "void* " ++ argName arg
      Ptr t     -> cRetType decl t ++ "* " ++ argName arg
      -- typedefs
      EventId   -> "int"
      -- special
      Vector ctp           -> "TVector" ++ ctypeSpec CInt ctp ++ argNameTuple
      Point ctp            -> "TPoint" ++ ctypeSpec CInt ctp ++ argNameTuple
      Size ctp             -> "TSize" ++ ctypeSpec CInt ctp ++  argNameTuple
      String ctp           -> "TString" ++ ctypeSpec CChar ctp  ++ " " ++ argName arg
      Rect ctp             -> "TRect" ++ ctypeSpec CInt ctp  ++  argNameTuple
      Fun _f               -> "TClosureFun "  ++ argName arg
      ArrayString ctp      -> "TArrayString" ++ ctypeSpec CChar ctp ++ " " ++ argName arg
      ArrayObject name ctp -> "TArrayObject" ++ ctypeSpec CObject ctp ++ "(" ++ name ++ ") " ++ argName arg
      RefObject name       -> "TClassRef(" ++  name ++ ") " ++ argName arg
      Object name | className' == name -> "TSelf(" ++  name ++ ") " ++ argName arg
                  | otherwise          -> "TClass(" ++  name ++ ") " ++ argName arg

      -- temporary types (can this ever happen?)
      StringLen               -> "TStringLen " ++ argName arg
      StringOut ctp           -> "TStringOut" ++ ctypeSpec CChar ctp ++ " " ++ argName arg
      ArrayLen                -> "TArrayLen " ++ argName arg
      ArrayStringOut ctp      -> "TArrayStringOut" ++ ctypeSpec CChar ctp ++ " " ++ argName arg
      ArrayObjectOut name ctp -> "TArrayObjectOut" ++ ctypeSpec CObject ctp ++ "(" ++ name ++ ") " ++ argName arg
      PointOut ctp            -> "TPointOut" ++ ctypeSpec CInt ctp ++ argNameTuple
      SizeOut ctp             -> "TSizeOut" ++ ctypeSpec CInt ctp ++ argNameTuple
      VectorOut ctp           -> "TVectorOut" ++ ctypeSpec CInt  ctp ++ argNameTuple
      RectOut ctp             -> "TRectOut" ++ ctypeSpec CInt ctp ++ argNameTuple

      _other  -> error ("cTypeArg: unknown argument type (" ++ show (argType arg) ++ ")") 
{-
      other  -> traceError ("unknown argument type (" ++ show (argType arg) ++ ")") decl $
                "ctypeSpec"
-}
  where
    argNameTuple
      = "(" ++ concat (intersperse "," (argNames arg)) ++ ")"


ctypeSpec :: CBaseType -> CBaseType -> String
ctypeSpec deftp ctp
  | deftp==ctp  = ""
  | otherwise   = case ctp of
                    CInt    -> "Int"
                    CLong   -> "Long"
                    CChar   -> "Char"
                    CVoid   -> "Void"
                    _other  -> ""
