module Main where

import Text.Parsec

import Plunge.Parsers.C
import Plunge.Parsers.PreprocessorOutput
import Plunge.Printers.PreprocessorOutput
import Plunge.Preprocessor

import System.Environment

main :: IO ()
main = do
  fName <- parseArguments
  cppResult <- preprocessFile fName []
  case cppResult of
    Left err  -> outputPreprocessorError err
    Right out -> runCppParser fName out >>= doStuff

  where
    doStuff parsed = do
      case parsed of
        Left err -> putStrLn $ "ERROR: " ++ (show err)
        Right result -> mapM_ (putStrLn . renderOriginal) result

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
