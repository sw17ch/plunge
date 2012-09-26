module Plunge.Parsers.PreprocessorOutput
  ( runCppParser
  , prettySection
  , Section(..)
  ) where

import Text.Parsec
import Control.Monad
-- import Control.Monad.Trans

type LineNumber = Int
data DirectiveFlag = EnterFile | ReturnFile | SystemHeader | ExternC
  deriving (Ord, Eq)
data CppDirective = CppDirective LineNumber FilePath [DirectiveFlag]

instance Show CppDirective where
  show (CppDirective n p fs) =
    concat ["# ", (show n), " \"", p, "\" ", (unwords $ map show fs)]

instance Show DirectiveFlag where
  show EnterFile    = "1"
  show ReturnFile   = "2"
  show SystemHeader = "3"
  show ExternC      = "4"

data Section
  = Block [String]
  | MiscDirective
      { directive :: CppDirective
      }
  | Expansion
      { enterDirective   :: CppDirective
      , returnDirective  :: CppDirective
      , sections         :: [Section]
      }
  deriving (Show)

type CppParser = ParsecT String () IO

showsSection :: Section -> ShowS
showsSection (Block ls) = foldr (.) id showss
  where
    showss = map shower ls
    shower l = \s -> l ++ s
showsSection (MiscDirective d) = (\s -> (show d) ++ s)
showsSection (Expansion od cd ss)
  = initial . (\s -> (show cd) ++ s)
  where
    initial = foldr (.) id $ ((shows od) : (map (indentedSection "  ") ss))
    indentedSection indent s = (shows indent) . (shows s)

prettySection :: Section -> String
prettySection s = showsSection s ""

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

runCppParser :: FilePath -> IO (Either ParseError [Section])
runCppParser path = do
  contents <- readFile path
  runParserT aCppFile () path contents

aCppFile :: CppParser [Section]
aCppFile = many aSection

aSection :: CppParser Section
aSection = (try aSectionMiscDirective)
       <|> (try aSectionExpansion)
       <|>      aSectionBlock

aSectionMiscDirective :: CppParser Section
aSectionMiscDirective = do
  (lineNum, fileName) <- aDirectivePreamble
  otherFlags          <- optionMaybe aMiscFlags
  _                   <- newline
  return $ MiscDirective {
    directive = CppDirective lineNum fileName (fromJustList otherFlags)
  }
  where
    fromJustList jlst = case jlst of
                             Nothing  -> []
                             Just lst -> lst


aSectionExpansion :: CppParser Section
aSectionExpansion = do
  ed         <- aEnterFileDirective
  (secs, rd) <- aSection `manyTillWithEnd` (try aReturnFileDirective)
  return $ Expansion
    { enterDirective  = ed
    , returnDirective = rd
    , sections        = secs
    }

aSectionBlock :: CppParser Section
aSectionBlock = liftM Block (many1 plainLine)
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

