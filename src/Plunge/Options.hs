module Plunge.Options ( PlungeCommand(..)
                      , optionInfo
                      ) where

import Paths_plunge

import Data.List
import Data.Monoid
import Data.Version
import Options.Applicative

data PlungeCommand = Correspond { inputFile     :: FilePath
                                , gccOptions    :: [String]
                                , linePadder   :: String
                                , emptyLine    :: String
                                , maxWidth     :: Maybe Int
                                -- , showLineNums :: Bool
                                -- , colorize     :: Bool
                                , verticalSep  :: String
                                , horizSep     :: String
                                } deriving (Show)

optParser :: Parser PlungeCommand
optParser = subparser (
    command "correspond" correspondInfo
  )

correspondParser :: Parser PlungeCommand
correspondParser = Correspond
  <$> strOption
      ( long "input-file" <> short 'i'
     <> metavar "FILE"
     <> help "The C file to analyze." )
  <*> (many $ strOption
      ( long "gcc-option" <> short 'g'
     <> metavar "OPTION"
     <> help "An option to pass to GCC. Can be specified multiple times." ))
  <*> option
      ( long "line-pad" <> short 'p'
     <> metavar "STRING"
     <> help "String to use to pad lines."
     <> value " "
     <> showDefault )
  <*> option
      ( long "empty-line" <> short 'e'
     <> metavar "STRING"
     <> help "String to use to represent empty lines"
     <> value "-"
     <> showDefault )
  <*> optional ( option
      ( long "max-width" <> short 'm'
     <> metavar "NUMBER"
     <> help "How wide each column of output is allowed to be"
     <> value 80
     <> showDefault ))
  <*> option
      ( long "vert-sep" <> short 'v'
     <> metavar "STRING"
     <> help "What string to use to separate the two columns"
     <> value " | "
     <> showDefault )
  <*> option
      ( long "horiz-sep" <> short 'h'
     <> metavar "STRING"
     <> help "What string to use to separate horizontal segments"
     <> value "-"
     <> showDefault )

optionInfo :: ParserInfo PlungeCommand
optionInfo = info (helper <*> optParser) (header summary_str)

correspondInfo :: ParserInfo PlungeCommand
correspondInfo = info (helper <*> correspondParser) (progDesc correspondDesc)


summary_str :: String
summary_str = let tags = versionTags version
                  branch_str = concat $ intersperse "." $ map show (versionBranch version)
                  tags_str = case tags of
                                [] -> ""
                                _  -> " (" ++ (concat $ intersperse ", " $ tags) ++ ")"
            in "Plunge " ++ branch_str ++ tags_str ++ ", (C) John Van Enk 2012"

correspondDesc :: String
correspondDesc = "Align the lines of the C file with the resulting lines after preprocessing."
