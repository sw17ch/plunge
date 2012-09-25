module Main where

import Plunge.Parsers.C
import Plunge.Parsers.PreprocessorOutput
import System.Environment

main :: IO ()
main = do
  [fName] <- getArgs
  s <- parsePreprocessedFile fName
  case s of
    Left err -> putStrLn $ "ERROR: " ++ (show err)
    Right result -> putStrLn $ prettySection result
