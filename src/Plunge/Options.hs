{-# LANGUAGE DeriveDataTypeable #-}
module Plunge.Options ( Options(..)
                      , defaultOpts
                      ) where

import Paths_plunge

import Data.Data
import Data.List
import Data.Version
import System.Console.CmdArgs

data Options = Options { inputFile    :: FilePath
                       , gccOptions   :: [String]
                       , linePadder   :: String
                       , emptyLine    :: String
                       , maxWidth     :: Maybe Int
                       -- , showLineNums :: Bool
                       -- , colorize     :: Bool
                       , verticalSep  :: String
                       , horizSep     :: String
                       } deriving (Show, Data, Typeable)

defaultOpts :: Options
defaultOpts = Options { inputFile = def      &= explicit &= name "input-file"
                                                         &= name "i"
                                             &= inputFile_help
                      , gccOptions = def     &= explicit &= name "gcc-option"
                                                         &= name "g"
                                             &= gccOptions_help
                      , linePadder = " "     &= explicit &= name "line-padding"
                                                         &= name "p"
                      , emptyLine = "."      &= explicit &= name "empty-line-padding"
                                                         &= name "e"
                      , maxWidth = Nothing   &= explicit &= name "max-width"
                                                         &= name "w"
                      -- , showLineNums = False &= name "show-line-numbers"
                      -- , colorize = False     &= name "colorize"
                      , verticalSep = " | "  &= explicit &= name "vertical-sep"
                                                         &= name "v"
                      , horizSep = "-"       &= explicit &= name "horizontal-sep"
                                                         &= name "h"
                      } &= program "plunge"
                        &= summary summary_str


summary_str :: String
summary_str = let tags = versionTags version
                  branch_str = concat $ intersperse "." $ map show (versionBranch version)
                  tags_str = case tags of
                                [] -> ""
                                _  -> " (" ++ (concat $ intersperse ", " $ tags) ++ ")"
            in "Plunge " ++ branch_str ++ tags_str ++ ", (C) John Van Enk 2012"


inputFile_help :: Ann
inputFile_help = help "The C file to analyze."

gccOptions_help :: Ann
gccOptions_help = help "An option to pass to GCC. Can be specified multiple times."
