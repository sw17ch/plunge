{-# LANGUAGE DeriveDataTypeable #-}
module Plunge.Options where

import Data.Data
import System.Console.CmdArgs

data Options = Options { inputFile     :: FilePath
                       , gccOptions    :: [String]
                       , linePadder    :: String
                       , emptyLineChar :: String
                       , maxWidth      :: Maybe Int
                       , showLineNums  :: Bool
                       , colorize      :: Bool
                       , separator     :: String
                       } deriving (Show, Data, Typeable)

defaultOpts :: Options
defaultOpts = Options { inputFile = def      &= name "input-file"
                      , gccOptions = []      &= name "gcc-option"
                      , linePadder = " "     &= name "line-padding"
                      , emptyLineChar = "."  &= name "empty-line-padding"
                      , maxWidth = Nothing   &= name "max-width"
                      , showLineNums = False &= name "show-line-numbers"
                      , colorize = False     &= name "colorize"
                      , separator = " | "    &= name "column-separator"
                      }

