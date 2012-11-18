module Main where

import Plunge.Parsers.PreprocessorOutput
import Plunge.Preprocessor
import Plunge.Analytics.C2CPP
import Plunge.Printers.Analytics
import Control.Monad

import System.Environment

type Options = Options { inputFile     :: Maybe String
                       , linePader     :: Char
                       , emptyLineChar :: Char
                       , maxWidth      :: Maybe Int
                       , showLineNums  :: Bool
                       , colorize      :: Bool
                       , separator     :: String
                       } deriving (Show)

defaultOpts :: Options
defaultOpts = Options { inputFile = Nothing
                      , linePadder = ' '
                      , emptyLineChar = '.'
                      , maxWidth = Nothing
                      , showLineNums = False
                      , colorize = False
                      , separator = " | "
                      }

main :: IO ()
main = do
  fPath <- parseArguments
  cppResult <- preprocessFile fPath []
  case cppResult of
    Left err  -> outputPreprocessorError err
    Right out -> parse fPath out

  where
    parse path cpp = do
      cLines <- liftM lines $ readFile path -- EEEEEEVIL
      parsed <- runCppParser path cpp
      case parsed of
        Left err -> putStrLn $ "ERROR: " ++ (show err)
        Right result -> analyze result cLines (lines cpp)
    analyze result cLines cppLines = do
      let assocs = lineAssociations result
      putStrLn $ renderAssociation assocs cLines cppLines

parseArguments :: IO FilePath
parseArguments = do
  [fName] <- getArgs
  return fName

outputPreprocessorError :: CppError -> IO ()
outputPreprocessorError e = do
  mapM_ putStrLn [ "C PREPROCESSOR ERROR"
                 , "--------------------"
                 , e
                 ]
