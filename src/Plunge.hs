module Main where

import Plunge.Parsers.C
import Plunge.Parsers.PreprocessorOutput
import Plunge.Printers.PreprocessorOutput
import System.Environment

main :: IO ()
main = do
  [fName] <- getArgs
  s <- runCppParser fName
  case s of
    Left err -> putStrLn $ "ERROR: " ++ (show err)
    Right result -> mapM_ (putStrLn . renderOriginal) result
