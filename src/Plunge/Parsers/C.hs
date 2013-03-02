module Plunge.Parsers.C where

import Language.C
import Language.C.System.GCC

walk :: CTranslUnit -> String
walk (CTranslUnit edecls _) = unlines $ map walkEdecl edecls

walkEdecl :: CExternalDeclaration NodeInfo -> String
walkEdecl (CDeclExt (CDecl _ _ i)) = "CDecl " ++ show i
walkEdecl (CFDefExt (CFunDef _ _ _ _ i)) = "CFunDef " ++ show i
walkEdecl (CAsmExt  _ i) = "CAsmExt " ++ show i

parseFile :: FilePath -> IO ()
parseFile path = do
  parsed <- parseCFile (newGCC "gcc") Nothing [] path
  case parsed of
    Left e -> putStrLn $ "Error: " ++ show e
    Right ctl -> putStrLn $ walk ctl
