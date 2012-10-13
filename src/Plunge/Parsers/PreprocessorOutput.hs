module Plunge.Parsers.PreprocessorOutput
  ( runCppParser
  , Section(..)
  ) where

import Text.Parsec
import Plunge.Types.PreprocessorOutput

type CppParser = ParsecT String LineNumber IO

--------------------------------------------------------------------------------

manyTillWithEnd :: (Stream s m t) => ParsecT s u m a
                                  -> ParsecT s u m end
                                  -> ParsecT s u m ([a], end)
manyTillWithEnd p end = go []
  where
    p_end cont = do
      e <- end
      return (reverse cont, e)
    p_next cont = do
      p' <- p
      go (p':cont)
    go cont = (p_end cont) <|> (p_next cont)

--------------------------------------------------------------------------------

runCppParser :: FilePath -> String -> IO (Either ParseError [Section])
runCppParser path contents = runParserT aCppFile 1 path contents

aCppFile :: CppParser [Section]
aCppFile = many aSection

aSection :: CppParser Section
aSection = (try aSectionMiscDirective)
       <|> (try aSectionExpansion)
       <|>      aSectionBlock

aSectionMiscDirective :: CppParser Section
aSectionMiscDirective = do
  p0 <- getPosition
  (lineNum, fileName) <- aDirectivePreamble
  otherFlags          <- optionMaybe aMiscFlags
  _                   <- newline
  p1 <- getPosition

  modifyState (\_ -> lineNum)
  return $ MiscDirective
    { directive = CppDirective lineNum fileName (fromJustList otherFlags)
    , lineRange = LineRange (sourceLine p0) (sourceLine p1)
    }
  where
    fromJustList jlst = case jlst of
                             Nothing  -> []
                             Just lst -> lst

aSectionExpansion :: CppParser Section
aSectionExpansion = do
  p0 <- getPosition
  num <- getState
  ed <- aEnterFileDirective
  (secs, rd) <- aSection `manyTillWithEnd` (try aReturnFileDirective)
  let (CppDirective rdNum _ _) = rd
  p1 <- getPosition

  modifyState (\_ -> rdNum)
  return $ Expansion
    { enterDirective  = ed
    , returnDirective = rd
    , startLine       = num
    , sections        = secs
    , lineRange       = LineRange (sourceLine p0) (sourceLine p1)
    }

aSectionBlock :: CppParser Section
aSectionBlock = do
  p0 <- getPosition
  startLn  <- getState
  ls <- many1 plainLine
  p1 <- getPosition

  modifyState (\n -> n + (length ls))
  return $ Block ls startLn $ LineRange (sourceLine p0) (sourceLine p1)
  where
    plainLine = do
      _ <- lookAhead $ noneOf "#"
      (l, nl) <- manyTillWithEnd anyChar (try newline)
      return $ l ++ [nl]

aEnterFileDirective :: CppParser CppDirective
aEnterFileDirective = do
  (lineNum, fileName) <- aDirectivePreamble
  _ <- char ' '
  enterFlag <- aEnterFile
  otherFlags <- aMiscFlags
  _ <- newline
  return $ CppDirective lineNum fileName (enterFlag:otherFlags)

aReturnFileDirective :: CppParser CppDirective
aReturnFileDirective = do
  (lineNum, fileName) <- aDirectivePreamble
  _ <- char ' '
  returnFlag <- aReturnFile
  otherFlags <- aMiscFlags
  _ <- newline
  return $ CppDirective lineNum fileName (returnFlag:otherFlags)

aDirectivePreamble :: CppParser (Int, String)
aDirectivePreamble = do
  _ <- string "# "
  lineNumStr <- many1 digit
  _ <- string " \""
  fileNameStr <- many1 (noneOf "\"")
  _ <- char '"'
  return $ (read lineNumStr, fileNameStr)

aEnterFile, aReturnFile, aSystemHeader, aExternC :: CppParser DirectiveFlag
aEnterFile    = char '1' >> return EnterFile
aReturnFile   = char '2' >> return ReturnFile
aSystemHeader = char '3' >> return SystemHeader
aExternC      = char '4' >> return ExternC

aMiscFlags :: CppParser [DirectiveFlag]
aMiscFlags = option [] someFlags
  where
    someFlags = do
      sh <- optionMaybe (char ' ' >> aSystemHeader)
      ec <- optionMaybe (char ' ' >> aExternC)
      case (sh, ec) of
        (Just sh', Just ec') -> return [sh', ec']
        (Just sh', Nothing ) -> return [sh'     ]
        (Nothing,  Just ec') -> return [     ec']
        (Nothing,  Nothing ) -> return [        ]

