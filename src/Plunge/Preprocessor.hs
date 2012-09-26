module Plunge.Preprocessor where

import System.Process
import System.Exit

type CppArg = String
type CppOutput = String
type CppError = String

preprocessFile :: FilePath -> [CppArg] -> IO (Either CppError CppOutput)
preprocessFile path args = do
  (ec, out, err) <- readProcessWithExitCode "gcc" args' ""

  case ec of
    ExitSuccess -> return $ Right out
    _           -> return $ Left err

  where
    args' = "-E":path:args
