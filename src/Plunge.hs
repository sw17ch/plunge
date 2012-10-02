module Main where

import Plunge.Parsers.PreprocessorOutput
import Plunge.Preprocessor
import Plunge.Analytics.C2CPP
import Plunge.Printers.Analytics

import System.Environment

main :: IO ()
main = do
  fPath <- parseArguments
  cppResult <- preprocessFile fPath []
  case cppResult of
    Left err  -> outputPreprocessorError err
    Right out -> parse fPath out

  where
    parse path cpp = do
      parsed <- runCppParser path cpp
      case parsed of
        Left err -> putStrLn $ "ERROR: " ++ (show err)
        Right result -> analyze path result
    analyze path parsed = do
      let ss = spans parsed
      cLines <- readFile path
      let pairs = pairSpans ss (map (++ "\n") $ lines cLines)
      let spanPairs = pairSpanLinesWithCLines pairs
      putStrLn $ renderC2Cpp spanPairs

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
