{-# LANGUAGe RecordWildCards #-}
module Main where

import Options.Applicative

import Plunge.Options
import Plunge.Parsers.PreprocessorOutput
import Plunge.Preprocessor
import Plunge.Analytics.C2CPP
import Plunge.Printers.Analytics

main :: IO ()
main = execParser optionInfo >>= runWithOptions

runWithOptions :: PlungeCommand -> IO ()
runWithOptions (Pointers {..}) = error "UNIMPLEMENTED"
runWithOptions (Correspond {..}) = doCorrespond inputFile
                                                gccOptions
                                                linePadder
                                                emptyLine
                                                maxWidth
                                                verticalSep
                                                horizSep

oCorrespond :: FilePath
            -> [CppArg]
            -> String
            -> String
            -> Maybe Int
            -> String
            -> String
            -> IO ()
doCorrespond _inputFile _gccOptions _linePadder _emptyLine _maxWidth _verticalSep _horizSep = do
  cppResult <- preprocessFile _inputFile _gccOptions
  cData <- readFile _inputFile

  case cppResult of
    Left err  -> outputPreprocessorError err
    Right cppData -> parse cData cppData
  where
    parse cData cppData = do
      parsed <- runCppParser _inputFile cppData
      case parsed of
        Left err -> putStrLn $ "ERROR: " ++ show err
        Right result -> analyze result (lines cData) (lines cppData)
    analyze result cLines cppLines = do
      let assocs = lineAssociations result
      putStrLn $ renderAssociation assocs cLines cppLines
                                   _linePadder
                                   _emptyLine
                                   _maxWidth
                                   _verticalSep
                                   _horizSep

outputPreprocessorError :: CppError -> IO ()
outputPreprocessorError e = 
  mapM_ putStrLn [ "C PREPROCESSOR ERROR"
                 , "--------------------"
                 , e
                 ]
