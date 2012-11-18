module Main where

import Plunge.Parsers.PreprocessorOutput
import Plunge.Types.PreprocessorOutput
import Plunge.Preprocessor
import Plunge.Analytics.C2CPP
import Plunge.Printers.Analytics
import Control.Monad

import System.Environment

import Text.Printf

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
      layout $ assocs
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

layout :: [LineAssociation] -> IO ()
layout assocs = do
  mapM_ pf assocs
  where
    pf assoc = printf "%50s :: %50s\n" (show $ cRange assoc) (show $ cppRange assoc)
