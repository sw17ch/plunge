module Plunge.Analytics.FindPtrs where

import Data.Typeable
import Data.Generics.Schemes
import Data.List
import Data.Maybe
import Language.C
import Language.C.Data.Ident
import Language.C.System.GCC

parseFile :: [String] -> FilePath -> IO (Either ParseError CTranslUnit)
parseFile = parseCFile (newGCC "gcc") Nothing

getDeclrs :: Typeable a => CTranslUnit -> [CDeclarator a]
getDeclrs = listify isDecl
  where
    isDecl :: Typeable a => CDeclarator a -> Bool
    isDecl _ = True

analyze :: CTranslUnit -> String
analyze ctu = let declrs = getDeclrs ctu :: [CDeclarator NodeInfo]
              in intercalate "\n" $ mapMaybe analyzeCDeclrs declrs

analyzeCDeclrs :: CDeclarator t -> Maybe String
analyzeCDeclrs (CDeclr Nothing _ _ _ _) = Nothing
analyzeCDeclrs (CDeclr (Just (Ident ident _ info)) attrs _ _ _) | isPtr attrs = Just rendering
                                                                | otherwise = Nothing
  where
    rendering = "POINTER: " ++ ident ++ " -- " ++ showFile (fileOfNode info) ++ ":" ++ show ((posRow . posOfNode) info)

isPtr :: [CDerivedDeclarator t] -> Bool
isPtr attrs | 0 < length [x | x@(CPtrDeclr _ _) <- attrs] = True
            | otherwise = False

showFile :: Maybe String -> String
showFile Nothing = "<undefined file>"
showFile (Just f) = f
