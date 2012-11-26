{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad
import System.Console.CmdArgs
import Data.Generics.Schemes
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC

data Options = Options { filePath :: FilePath
                       , cppFlags :: [String]
                       , cppFlagFile :: Maybe FilePath
                       } deriving (Data, Typeable, Show)

defOpts :: Options
defOpts = Options { filePath = def, cppFlags = def, cppFlagFile = def }

main :: IO ()
main = do
    opts <- cmdArgs defOpts

    flagsFromFile <- case cppFlagFile opts of
                          Nothing -> return []
                          Just f -> liftM lines $ readFile f

    parsed <- parseCFile (newGCC "gcc") Nothing (flagsFromFile ++ (cppFlags opts)) (filePath opts)

    case parsed of
        Left e -> error $ "Unable to parse file:" ++ (show e)
        Right result -> analyze result

getDeclrs :: Typeable a => CTranslUnit -> [CDeclarator a]
getDeclrs ctu = (listify isDecl) ctu
  where
    isDecl :: Typeable a => CDeclarator a -> Bool
    isDecl _ = True

analyze :: CTranslUnit -> IO ()
analyze ctu = putStrLn $ concatMap analyzeCDeclrs (getDeclrs ctu :: [CDeclarator NodeInfo])

analyzeCDeclrs :: CDeclarator t -> [Char]
analyzeCDeclrs (CDeclr Nothing _ _ _ _) = ""
analyzeCDeclrs (CDeclr (Just (Ident ident _ info)) attrs _ _ _) | isPtr attrs = "POINTER: " ++ ident ++ " -- " ++ (showFile $ fileOfNode info) ++ ":" ++ (show $ posRow $ posOfNode info) ++ "\n"
                                                                | otherwise = ""

isPtr :: [CDerivedDeclarator t] -> Bool
isPtr attrs | 0 < length [x | x@(CPtrDeclr _ _) <- attrs] = True
            | otherwise = False

showFile :: Maybe [Char] -> [Char]
showFile Nothing = "<undefined file>"
showFile (Just f) = f
