module Plunge.Parsers.PreprocessorOutput
  ( parsePreprocessedFile
  , prettySection
  , Section(..)
  ) where

import Text.Parsec
import Control.Monad
import Control.Monad.Trans

------------------------------
type CPPParser = ParsecT String () IO
type LineNumber = Int
data CPPDirective = CPPDirective LineNumber FilePath [DirectiveFlag]
  deriving (Show)

data CPPLine = PlainLine String
             | Directive CPPDirective
  deriving (Show)

data DirectiveFlag = EnterFile
                   | ReturnFile
                   | SystemHeader
                   | ExternC
  deriving (Show, Ord, Eq)

data Section = Block { lines        :: [String],
                       startLineNum :: Int,
                       endLineNum   :: Int }
             | Expansion { sections     :: [Section],
                           filePath     :: FilePath,
                           startLineNum :: Int,
                           endLineNum   :: Int,
                           flags        :: [DirectiveFlag] }
  deriving (Show)
------------------------------
prettySection :: Section -> String
prettySection s = unlines $ prettySection' "" s
  where
    prettySection' :: String -> Section -> [String]
    prettySection' indent s =
      case s of
        (Block ls _ _)          -> prettyBlockLines indent ls
        (Expansion ss p _ _ fs) -> prettyExpansion indent ss p fs
    prettyBlockLines indent ls =
      map (indent ++) ls
    prettyExpansion indent ss p fs =
      (indent ++ "SECTION " ++ p) : (concatMap (prettySection' ("  " ++ indent)) ss)
------------------------------

parsePreprocessedFile :: FilePath -> IO (Either ParseError Section)
parsePreprocessedFile path = do
  contents <- readFile path
  runParserT parseManyExpansions () path contents

parsePlainLine :: CPPParser String
parsePlainLine = do
  _    <- lookAhead $ noneOf "#"
  rest <- manyTill anyChar (try $ lookAhead newline)
  _    <- newline
  return rest

parseMiscDirectiveFlag :: CPPParser DirectiveFlag
parseMiscDirectiveFlag = choice [ parseSystemHeader,
                                  parseExternC ]

parseEnterFile :: CPPParser DirectiveFlag
parseEnterFile = char '1' >> return EnterFile

parseReturnFile :: CPPParser DirectiveFlag
parseReturnFile = char '2' >> return ReturnFile

parseSystemHeader :: CPPParser DirectiveFlag
parseSystemHeader = char '3' >> return SystemHeader

parseExternC :: CPPParser DirectiveFlag
parseExternC = char '4' >> return ExternC

parseDirectivePreamble :: CPPParser (String, String)
parseDirectivePreamble = do
  _ <- char '#'
  _ <- char ' '
  lineNumStr <- many1 digit
  _ <- char ' '
  _ <- char '"'
  fileNameStr <- many1 (noneOf "\"")
  _ <- char '"'
  return $ (lineNumStr, fileNameStr)

parseDirectiveWithEnterFile :: CPPParser CPPDirective
parseDirectiveWithEnterFile = do
  (lineNumStr, fileNameStr) <- parseDirectivePreamble
  _ <- char ' '
  enterFlag <- parseEnterFile
  otherFlags <- (optional $ char ' ') >> (parseMiscDirectiveFlag `sepBy` char ' ')
  _ <- newline

  lift $ putStrLn "Parse Directive With Enter File"
  return $ CPPDirective (read lineNumStr) fileNameStr (enterFlag : otherFlags)

parseDirectiveWithReturnFile :: CPPParser CPPDirective
parseDirectiveWithReturnFile = do
  (lineNumStr, fileNameStr) <- parseDirectivePreamble
  _ <- char ' '
  enterFlag <- parseReturnFile
  otherFlags <- (optional $ char ' ') >> (parseMiscDirectiveFlag `sepBy` char ' ')
  _ <- newline

  lift $ putStrLn "Parse Directive With Return File"
  return $ CPPDirective (read lineNumStr) fileNameStr (enterFlag : otherFlags)


parseBoringDirective :: CPPParser CPPDirective
parseBoringDirective = do
  (lineNumStr, fileNameStr) <- parseDirectivePreamble
  flags' <- (optional $ char ' ') >> ((parseSystemHeader <|> parseExternC) `sepBy` char ' ')
  _ <- newline

  lift $ putStrLn "Parse Boring Directive"
  return $ CPPDirective (read lineNumStr) fileNameStr flags'

parseManyExpansions :: CPPParser Section
parseManyExpansions = do
  exps <- many parseExpansion
  return $ Expansion {
    sections = exps,
    filePath = "something",
    startLineNum = 0,
    endLineNum = 0,
    flags = []
  }

parseExpansion :: CPPParser Section
parseExpansion = (try parseBoringExpansion)
             <|> (try parseEnterExpansion)
             <|> parseReturnExpansion

parseBoringExpansion :: CPPParser Section
parseBoringExpansion = do
  (CPPDirective lineNum path flags') <- parseBoringDirective
  return $ Expansion {
    sections     = [],
    filePath     = path,
    startLineNum = lineNum,
    endLineNum   = lineNum,
    flags        = flags'
  }

parseEnterExpansion :: CPPParser Section
parseEnterExpansion = do
  CPPDirective lineNum path flags' <- parseDirectiveWithEnterFile
  secs <- parseEnterSections

  lift $ putStrLn "Parse Enter Expansion"
  return $ Expansion {
    sections     = secs,
    filePath     = path,
    startLineNum = lineNum,
    endLineNum   = lineNum,
    flags        = flags'
  }

parseReturnExpansion :: CPPParser Section
parseReturnExpansion = do
  CPPDirective lineNum path flags' <- parseDirectiveWithReturnFile
  secs <- parseReturnSections

  lift $ putStrLn "Parse Return Expansion"
  return $ Expansion {
    sections     = secs,
    filePath     = path,
    startLineNum = lineNum,
    endLineNum   = lineNum,
    flags        = flags'
  }

parseBlock :: CPPParser Section
parseBlock = do
  lift $ putStrLn "Parse Block"
  plainLines <- many1 parsePlainLine
  return $ Block plainLines 0 0

parseEnterSections :: CPPParser [Section]
parseEnterSections = do
  lift $ putStrLn "Parse Enter Sections"
  manyTill (parseBlock <|> parseExpansion) termination
  where
    termination = try $ lookAhead parseDirectiveWithReturnFile

parseReturnSections :: CPPParser [Section]
parseReturnSections = do
  lift $ putStrLn "Parse Return Sections"
  manyTill (parseBlock <|> parseExpansion) termination
  where
    termination = try $ lookAhead returnTermination

returnTermination = (liftM Left eof) <|> (liftM Right parseDirectiveWithEnterFile)
