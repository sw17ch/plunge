module Main where

-- import Text.Parsec
-- import Plunge.Parsers.C
-- import Plunge.Printers.PreprocessorOutput

import Plunge.Parsers.PreprocessorOutput
import Plunge.Preprocessor
import Plunge.Analytics.C2CPP

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
        Right result -> analyze result
    analyze parsed = print $ spans parsed

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
