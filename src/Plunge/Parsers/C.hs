module Plunge.Parsers.C where

import Language.C
import Language.C.System.GCC

walk :: CTranslUnit -> String
walk (CTranslUnit edecls info) = unlines $ map walk_edecl edecls

walk_edecl :: CExternalDeclaration NodeInfo -> String
walk_edecl (CDeclExt (CDecl _ _ i)) = "CDecl " ++ show i
walk_edecl (CFDefExt (CFunDef _ _ _ _ i)) = "CFunDef " ++ show i
walk_edecl (CAsmExt  _ i) = "CAsmExt " ++ show i

parseFile :: FilePath -> IO ()
parseFile path = do
  parsed <- parseCFile (newGCC "gcc") Nothing [] path
  case parsed of
    Left e -> putStrLn $ "Error: " ++ (show e)
    Right ctl -> putStrLn $ walk ctl
