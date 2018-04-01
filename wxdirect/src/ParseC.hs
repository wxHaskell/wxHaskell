{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------------------
{-| Module      :  ParseC
    Copyright   :  (c) Daan Leijen 2003
    License     :  BSD-style

    Maintainer  :  wxhaskell-devel@lists.sourceforge.net
    Stability   :  provisional
    Portability :  portable

    Parse the wxc C header files.
-}
-----------------------------------------------------------------------------------------
module ParseC( parseC, readHeaderFile ) where

import Data.Char( isSpace )
import Data.List( isPrefixOf )
import Data.Maybe( isJust )
#if __GLASGOW_HASKELL__ < 710
import Data.Functor( (<$>) )
#endif
import System.Process( readProcess )
import System.Environment (lookupEnv)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Types


{-----------------------------------------------------------------------------------------
   Parse C
-----------------------------------------------------------------------------------------}
parseC :: FilePath -> IO [Decl]
parseC fname
  = do contents <- readHeaderFile fname
       declss   <- mapM (parseDecl fname) (pairComments  contents)
       return (concat declss)


readHeaderFile :: FilePath -> IO [String]
readHeaderFile fname =
  do
    includeDirectories <- getIncludeDirectories
    putStrLn ("Preprocessing and parsing file: " ++ fname ++ 
              ",\n  using include directories: " ++ (unwords includeDirectories)) 
    flattenComments .
      filter (not . isPrefixOf "//") .
      filter (not . isPrefixOf "#")  .
      lines <$>
      readProcess 
        "cpp"
        ( includeDirectories ++ 
          [ "-C"              -- Keep the comments
          , "-DWXC_TYPES_H"   -- Make sure wxc_types.h is not preprocessed, 
                              -- so the type macros are not replaced 
                              -- (the parser scans for certain macros)
          , fname             -- The file to process
          ]
        )
        ""

readShellProcess :: FilePath -> [String] -> IO String
readShellProcess f as = readProcess "sh" (f:as) "" 

isMsys :: IO Bool
isMsys = isJust <$> lookupEnv "MSYSTEM"

deMsysPaths :: String -> IO String
deMsysPaths s = head . lines <$> readProcess "sh" ["-c", "cd " ++ s ++ "; pwd -W"] ""

getIncludeDirectories :: IO [String]
getIncludeDirectories = do
  im <- isMsys
  if im
    then readShellProcess "wx-config" ["--cppflags"] >>= 
            mapM (fmap ("-I"++) . deMsysPaths . drop 2) . filter (isPrefixOf "-I") . words
    else
        filter (isPrefixOf "-I") . words <$> 
            readProcess "wx-config" ["--cppflags"] ""
    
                      
-- flaky, but suitable
flattenComments :: [String] -> [String]
flattenComments inputLines
  = case inputLines of
      (('/':'*':_):_) -> let (incomment,comment:rest) = span (not . endsComment) inputLines
                         in (concat (incomment ++ [comment]) : flattenComments rest)
      xs : xss        -> xs : flattenComments xss
      []              -> []
  where
    endsComment line  = isPrefixOf "/*" (dropWhile isSpace (reverse line))
                     

pairComments :: [String] -> [(String,String)]
pairComments inputLines
  = case inputLines of
      ('/':'*':'*':xs) : ys : xss  | not (classDef ys) -> (reverse (drop 2 (reverse xs)),ys) : pairComments xss
      xs : xss                     | not (classDef xs) -> ("",xs) : pairComments xss
                                   | otherwise         -> pairComments xss
      []                           -> []
  where
    classDef xs   = isPrefixOf "TClassDef" xs

parseDecl :: FilePath -> (String,String) -> IO [Decl]
parseDecl fname (comment,line)
  = case parse pdecl fname line of
      Left err  -> do putStrLn ("ignore: parse error : " ++ show err ++ ", on : " ++ line)
                      return []
      Right mbd -> case mbd of
                     Just d  -> return [d{ declComment = comment }]
                     Nothing -> return []     -- empty line


{-----------------------------------------------------------------------------------------
   Parse declaration
-----------------------------------------------------------------------------------------}
-- parse a declaration: return Nothing on an empty declaration
pdecl :: Parser (Maybe Decl)
pdecl
  = do whiteSpace
       x <- (do f <- pfundecl; return (Just f)) <|> return Nothing
       eof
       return x

pfundecl :: Parser Decl
pfundecl
  = do optional (reserved "EXPORT")
       declRet' <- ptype
       optional (reserved "_stdcall" <|> reserved "__cdecl")
       declName' <- identifier <?> "function name"
       declArgs' <- pargs
       _         <- semi
       return (Decl declName' declRet' declArgs' "")
  <?> "function declaration"

pargs :: Parser [Arg]
pargs
  = parens (commaSep parg)
  <?> "arguments"

parg :: Parser Arg
parg
  =   pargTypes
  <|> do argType' <- ptype
         argName' <- identifier
         return (Arg [argName'] argType')
  <?> "argument"

ptype :: Parser Type
ptype
  = do tp    <- patomtype
       stars <- many (symbol "*")
       return (foldr (\_ tp' -> Ptr tp') tp stars)
  <?> "type"

patomtype :: Parser Type
patomtype
  =   do reserved "void"; return Void
  <|> do reserved "int";  return (Int CInt)
  <|> do reserved "char"; return Char
  <|> do reserved "long";  return (Int CLong)
  <|> do reserved "double"; return Double
  <|> do reserved "float";  return Float
  <|> do reserved "size_t"; return (Int SizeT)
  <|> do reserved "time_t"; return (Int TimeT)
  <|> do reserved "uint8_t";  return Word8
  <|> do reserved "uint32_t"; return Word32
  <|> do reserved "uint64_t"; return Word64
  <|> do reserved "intptr_t"; return IntPtr
  <|> do reserved "int64_t";  return Int64
  <|> do reserved "TIntPtr";  return IntPtr
  <|> do reserved "TInt64"; return Int64
  <|> do reserved "TUInt"; return Word
  <|> do reserved "TUInt8"; return Word8
  <|> do reserved "TUInt32"; return Word32
  <|> do reserved "TBool";   return Bool
  <|> do reserved "TBoolInt"; return Bool
  <|> do reserved "TChar";   return Char
  <|> do reserved "TString"; return (String CChar)
  <|> do reserved "TStringVoid"; return (String CVoid)
  <|> do reserved "TStringOut"; return (StringOut CChar)
  <|> do reserved "TStringOutVoid"; return (StringOut CVoid)
  <|> do reserved "TStringLen"; return StringLen
  <|> do reserved "TByteData"; return Char
  <|> do reserved "TByteStringOut"; return (ByteStringOut Strict)
  <|> do reserved "TByteStringLazyOut"; return (ByteStringOut Lazy)
  <|> do reserved "TByteStringLen"; return ByteStringLen
  <|> do reserved "TArrayLen"; return ArrayLen
  <|> do reserved "TArrayStringOut"; return (ArrayStringOut CChar)
  <|> do reserved "TArrayStringOutVoid"; return (ArrayStringOut CVoid)
  <|> do reserved "TArrayIntOut"; return (ArrayIntOut CInt)
  <|> do reserved "TArrayIntPtrOut"; return (ArrayIntPtrOut CInt)
  <|> do reserved "TArrayIntOutVoid"; return (ArrayIntOut CVoid)
  <|> do reserved "TArrayIntPtrOutVoid"; return (ArrayIntPtrOut CVoid)
  <|> do reserved "TClosureFun"; return (Fun "Ptr fun -> Ptr state -> Ptr (TEvent evt) -> IO ()")
  <|> do reserved "TClass"
         name <- parens identifier
         return (Object name)
  <|> do reserved "TSelf"
         name <- parens identifier
         return (Object name)
  <|> do reserved "TClassRef"
         name <- parens identifier
         return (RefObject name)
  <|> do reserved "TArrayObjectOut"
         name <- parens identifier
         return (ArrayObjectOut name CObject)
  <|> do reserved "TArrayObjectOutVoid"
         name <- parens identifier
         return (ArrayObjectOut name CVoid)


pargTypes :: Parser Arg
pargTypes
  = do tp       <- pargType2
       argnames <- parens pargs2
       return (Arg argnames tp)
  <|>
    do tp       <- pargType3
       argnames <- parens pargs3
       return (Arg argnames tp)
  <|>
    do tp       <- pargType4
       argnames <- parens pargs4
       return (Arg argnames tp)
  <|>
    do reserved "TArrayObject"
       parens  (do n  <- identifier
                   _  <- comma
                   tp <- identifier
                   _  <- comma
                   p  <- identifier
                   return (Arg [n,p] (ArrayObject tp CVoid)))

pargs2 :: Parser [String]
pargs2
  = do a1 <- identifier
       _  <- comma
       a2 <- identifier
       return [a1,a2]

pargs3 :: Parser [String]
pargs3
  = do a1 <- identifier
       _  <- comma
       a2 <- identifier
       _  <- comma
       a3 <- identifier
       return [a1,a2,a3]

pargs4 :: Parser [String]
pargs4
  = do a1 <- identifier
       _  <- comma
       a2 <- identifier
       _  <- comma
       a3 <- identifier
       _  <- comma
       a4 <- identifier
       return [a1,a2,a3,a4]


pargType2 :: Parser Type
pargType2
  =   do reserved "TPoint";  return (Point CInt)
  <|> do reserved "TSize";   return (Size CInt)
  <|> do reserved "TVector"; return (Vector CInt)
  <|> do reserved "TPointDouble"; return (Point CDouble)
  <|> do reserved "TPointLong"; return (Point CLong)
  <|> do reserved "TSizeDouble";   return (Size CDouble)
  <|> do reserved "TVectorDouble"; return (Vector CDouble)
  <|> do reserved "TPointOut";  return (PointOut CInt)
  <|> do reserved "TSizeOut";   return (SizeOut CInt)
  <|> do reserved "TVectorOut"; return (VectorOut CInt)
  <|> do reserved "TPointOutDouble";  return (PointOut CDouble)
  <|> do reserved "TPointOutVoid";  return (PointOut CVoid)
  <|> do reserved "TSizeOutDouble";   return (SizeOut CDouble)
  <|> do reserved "TSizeOutVoid";   return (SizeOut CVoid)
  <|> do reserved "TVectorOutDouble"; return (VectorOut CDouble)
  <|> do reserved "TVectorOutVoid"; return (VectorOut CVoid)
  <|> do reserved "TArrayString"; return (ArrayString CChar)
  <|> do reserved "TArrayInt"; return (ArrayInt CInt)
  <|> do reserved "TArrayIntPtr"; return (ArrayIntPtr CInt)
  <|> do reserved "TByteString"; return (ByteString Strict)
  <|> do reserved "TByteStringLazy"; return (ByteString Lazy)

pargType3 :: Parser Type
pargType3
  =   do reserved "TColorRGB"; return (ColorRGB CChar)

pargType4 :: Parser Type
pargType4
  =   do reserved "TRect"; return (Rect CInt)
  <|> do reserved "TRectDouble"; return (Rect CDouble)
  <|> do reserved "TRectOut"; return (RectOut CInt)
  <|> do reserved "TRectOutDouble"; return (RectOut CDouble)
  <|> do reserved "TRectOutVoid"; return (RectOut CVoid)


{-----------------------------------------------------------------------------------------
   The lexer
-----------------------------------------------------------------------------------------}
lexer :: P.TokenParser ()
lexer
  = P.makeTokenParser $
    emptyDef
    { commentStart = "/*"
    , commentEnd   = "*/"
    , commentLine  = "#"          -- ignore pre-processor stuff, but fail to recognise "//"
    , nestedComments = False
    , identStart   = letter <|> char '_'
    , identLetter  = alphaNum <|> oneOf "_'"
    , caseSensitive = True
    , reservedNames = ["void","int","long","float","double","char","size_t","time_t","_stdcall","__cdecl"
                      ,"TChar","TBool"
                      ,"TIntPtr"
                      ,"TClass","TSelf","TClassRef"
                      ,"TByteData","TByteString","TByteStringOut","TByteStringLen"
                      ,"TString","TStringOut","TStringLen", "TStringVoid"
                      ,"TPoint","TSize","TVector","TRect"
                      ,"TPointOut","TSizeOut","TVectorOut","TRectOut"
                      ,"TPointOutVoid","TSizeOutVoid","TVectorOutVoid","TRectOutVoid"
                      ,"TClosureFun"
                      ,"TPointDouble", "TPointLong", "TSizeDouble", "TVectorDouble", "TRectDouble"
                      ,"TPointOutDouble", "TSizeOutDouble", "TVectorOutDouble", "TRectOutDouble"
                      ,"TArrayLen","TArrayStringOut","TArrayStringOutVoid","TArrayObjectOut","TArrayObjectOutVoid"
                      ,"TColorRGB"
                      ,"EXPORT"
                      ]
    }

whiteSpace    :: Parser ()
whiteSpace    = P.whiteSpace lexer

symbol        :: String -> Parser String
symbol        = P.symbol lexer

parens        :: Parser a -> Parser a
parens        = P.parens lexer

semi          :: Parser String
semi          = P.semi lexer

comma         :: Parser String
comma         = P.comma lexer

commaSep      :: Parser a -> Parser [a]
commaSep      = P.commaSep lexer

identifier    :: Parser String
identifier    = P.identifier lexer

reserved      :: String -> Parser ()
reserved      = P.reserved lexer
