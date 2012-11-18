{-# LANGUAGE DeriveDataTypeable #-}
module Plunge.Options where

import Data.Data
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
defaultOpts = Options { inputFile = def      &= name "input-file"
                      , gccOptions = []      &= name "gcc-option"
                      , linePadder = " "     &= name "line-padding"
                      , emptyLine = "."      &= name "empty-line-padding"
                      , maxWidth = Nothing   &= name "max-width"
                      -- , showLineNums = False &= name "show-line-numbers"
                      -- , colorize = False     &= name "colorize"
                      , verticalSep = " | "  &= name "vertical-sep"
                      , horizSep = "-"       &= name "horizontal-sep"
                      }

