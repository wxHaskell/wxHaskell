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
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Types

{-----------------------------------------------------------------------------------------
   Parse C
-----------------------------------------------------------------------------------------}
parseC :: FilePath -> IO [Decl]
parseC fname
  = do lines  <- readHeaderFile fname
       declss <- mapM (parseDecl fname) (pairComments lines)
       -- putStrLn ("ok.")
       return (concat declss)

-- flaky but suitable.
readHeaderFile :: FilePath -> IO [String]
readHeaderFile fname
  = do putStrLn ("parsing: " ++ fname)
       input <- readFile fname
       lls   <- mapM readIncludeFile (flattenComments (lines input))
       return (concat lls)
  where
    pathName
      = reverse $ dropWhile (\c -> not (elem c "/\\")) $ reverse fname

    readIncludeFile line
      | isPrefixOf "#include \"" line  
      = readHeaderFile (pathName ++ includePath)
      where
        includePath = takeWhile (/='"') $ tail $ dropWhile (/='"') line

    readIncludeFile line
      = return [line]
                        
-- flaky, but suitable
flattenComments :: [String] -> [String]
flattenComments lines
  = case lines of
      (('/':'*':xs):xss) -> let (incomment,comment:rest) = span (not . endsComment) lines
                         in (concat (incomment ++ [comment]) : flattenComments rest)
      xs : xss        -> xs : flattenComments xss
      []              -> []
  where
    endsComment line  = isPrefixOf "/*" (dropWhile isSpace (reverse line))
                     

pairComments :: [String] -> [(String,String)]
pairComments lines
  = case lines of
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
       declRet <- ptype
       optional (reserved "_stdcall" <|> reserved "__cdecl")
       declName <- identifier <?> "function name"
       declArgs <- pargs
       semi
       return (Decl declName declRet declArgs "")
  <?> "function declaration"

pargs :: Parser [Arg]
pargs
  = parens (commaSep parg)
  <?> "arguments"

parg :: Parser Arg
parg
  =   pargTypes
  <|> do argType <- ptype
         argName <- identifier
         return (Arg [argName] argType)
  <?> "argument"

ptype :: Parser Type
ptype
  = do tp    <- patomtype
       stars <- many (symbol "*")
       return (foldr (\_ tp -> Ptr tp) tp stars)
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
       parens  (do n <- identifier
                   comma
                   tp <- identifier
                   comma
                   p <- identifier
                   return (Arg [n,p] (ArrayObject tp CVoid)))

pargs2
  = do a1 <- identifier
       comma
       a2 <- identifier
       return [a1,a2]

pargs3
  = do a1 <- identifier
       comma
       a2 <- identifier
       comma
       a3 <- identifier
       return [a1,a2,a3]

pargs4
  = do a1 <- identifier
       comma
       a2 <- identifier
       comma
       a3 <- identifier
       comma
       a4 <- identifier
       return [a1,a2,a3,a4]


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

pargType3
  =   do reserved "TColorRGB"; return (ColorRGB CChar)

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

whiteSpace    = P.whiteSpace lexer
lexeme        = P.lexeme lexer
symbol        = P.symbol lexer
parens        = P.parens lexer
semi          = P.semi lexer
comma         = P.comma lexer
commaSep      = P.commaSep lexer
identifier    = P.identifier lexer
reserved      = P.reserved lexer
