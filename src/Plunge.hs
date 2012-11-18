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

  cppResult <- preprocessFile (inputFile opts) []
  cData <- readFile (inputFile opts)

  case cppResult of
    Left err  -> outputPreprocessorError err
    Right cppData -> parse (inputFile opts) cData cppData

parse :: FilePath -> String -> String -> IO ()
parse fileName cData cppData = do
  parsed <- runCppParser fileName cppData
  case parsed of
    Left err -> putStrLn $ "ERROR: " ++ (show err)
    Right result -> analyze result (lines cData) (lines cppData)

analyze :: [Section] -> [CLine] -> [CppLine] -> IO ()
analyze result cLines cppLines = do
  let assocs = lineAssociations result
  putStrLn $ renderAssociation assocs cLines cppLines

outputPreprocessorError :: CppError -> IO ()
outputPreprocessorError e = do
  mapM_ putStrLn [ "C PREPROCESSOR ERROR"
                 , "--------------------"
                 , e
                 ]
