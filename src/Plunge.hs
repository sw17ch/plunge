module Main where

import System.Console.CmdArgs

import Plunge.Options
import Plunge.Parsers.PreprocessorOutput
import Plunge.Preprocessor
import Plunge.Analytics.C2CPP
import Plunge.Printers.Analytics
import Plunge.Types.PreprocessorOutput

main :: IO ()
main = do
  opts <- cmdArgs defaultOpts

  cppResult <- preprocessFile (inputFile opts) (gccOptions opts)
  cData <- readFile (inputFile opts)

  case cppResult of
    Left err  -> outputPreprocessorError err
    Right cppData -> parse opts (inputFile opts) cData cppData

parse :: Options -> FilePath -> String -> String -> IO ()
parse opts fileName cData cppData = do
  parsed <- runCppParser fileName cppData
  case parsed of
    Left err -> putStrLn $ "ERROR: " ++ (show err)
    Right result -> analyze opts result (lines cData) (lines cppData)

analyze :: Options -> [Section] -> [CLine] -> [CppLine] -> IO ()
analyze opts result cLines cppLines = do
  let assocs = lineAssociations result
  putStrLn $ renderAssociation opts assocs cLines cppLines

outputPreprocessorError :: CppError -> IO ()
outputPreprocessorError e = do
  mapM_ putStrLn [ "C PREPROCESSOR ERROR"
                 , "--------------------"
                 , e
                 ]
