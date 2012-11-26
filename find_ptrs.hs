{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad
import System.Console.CmdArgs
import Data.Generics.Schemes
import Data.List
import Data.Maybe
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
        Right result -> putStrLn $ analyze result

getDeclrs :: Typeable a => CTranslUnit -> [CDeclarator a]
getDeclrs ctu = (listify isDecl) ctu
  where
    isDecl :: Typeable a => CDeclarator a -> Bool
    isDecl _ = True

analyze :: CTranslUnit -> String
analyze ctu = let declrs = getDeclrs ctu :: [CDeclarator NodeInfo]
              in concat $ intersperse "\n" $ mapMaybe analyzeCDeclrs declrs

analyzeCDeclrs :: CDeclarator t -> Maybe String
analyzeCDeclrs (CDeclr Nothing _ _ _ _) = Nothing
analyzeCDeclrs (CDeclr (Just (Ident ident _ info)) attrs _ _ _) | isPtr attrs = Just rendering
                                                                | otherwise = Nothing
  where
    rendering = "POINTER: " ++ ident ++ " -- " ++ (showFile $ fileOfNode info) ++ ":" ++ (show $ posRow $ posOfNode info)

isPtr :: [CDerivedDeclarator t] -> Bool
isPtr attrs | 0 < length [x | x@(CPtrDeclr _ _) <- attrs] = True
            | otherwise = False

showFile :: Maybe [Char] -> [Char]
showFile Nothing = "<undefined file>"
showFile (Just f) = f
