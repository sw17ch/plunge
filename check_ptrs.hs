{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Monad
import System.Console.CmdArgs
import Language.C
import Language.C.Analysis
import Language.C.Data.Ident
import Language.C.Syntax.AST
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

analyze (CTranslUnit decls _) = do
    -- putStrLn $ unlines $ map show decls
    putStrLn $ concat $ map analyzeExt decls

analyzeExt (CDeclExt decl) = analyzeCDecl decl
analyzeExt (CFDefExt fdec) = analyzeFunDef fdec
analyzeExt (CAsmExt _ _) = "literal asm"

analyzeCDecl  (CDecl specs decls d) = (concat $ map analyzeSpecs specs) ++ (concat $ map analyzeCDecls decls)
analyzeFunDef (CFunDef _ decl _ stmt _) = concat (analyzeDeclr decl) ++ (analyzeCStatement stmt) ++ "\n"

analyzeDeclr (CDeclr ident dirDecls _ attrs info) = map analyzeDerived dirDecls

analyzeDerived (CPtrDeclr quals info) = "" -- show quals
analyzeDerived (CArrDeclr quals size info) = "" -- show quals
analyzeDerived (CFunDeclr (Left idnts) _ info) = ""
analyzeDerived (CFunDeclr (Right (decls, flag)) _ info) = concat $ map analyzeCDecl decls

analyzeCDecls (Nothing, _, _) = ""
analyzeCDecls (Just (CDeclr Nothing _ _ _ _), _, _) = ""
analyzeCDecls (Just (CDeclr (Just (Ident ident _ info)) attrs _ _ _), _, _) | isPtr attrs = "POINTER: " ++ ident ++ " -- " ++ (showFile $ fileOfNode info) ++ ":" ++ (show $ posRow $ posOfNode info) ++ "\n"
                                                                            | otherwise = ""

analyzeSpecs (CTypeSpec s) =  analyzeCTypeSpec s
analyzeSpecs _ = ""

analyzeCTypeSpec (CSUType s _) = analyzeCStruct s
analyzeCTypeSpec _ = ""

analyzeCStruct (CStruct _ _ (Just decl) _ _) = concat $ map analyzeCDecl decl
analyzeCStruct _ = ""

analyzeCStatement (CLabel _ s _ _) = ""
analyzeCStatement (CCase e s _) = ""
analyzeCStatement (CCases e e' s _) = ""
analyzeCStatement (CDefault s _) = ""
analyzeCStatement (CExpr Nothing _) = ""
analyzeCStatement (CExpr (Just e) _) = ""
analyzeCStatement (CCompound _ items _) = concat $ map analyzeCCompound items
analyzeCStatement (CIf e s (Nothing) _) = ""
analyzeCStatement (CIf e s (Just s') _) = ""
analyzeCStatement (CSwitch e s _) = ""
analyzeCStatement (CWhile e s _ _) = ""
analyzeCStatement (CFor (Left Nothing) _ _ s _) = ""  -- this needs to expand out a lot
analyzeCStatement (CFor (Left (Just e)) _ _ s _) = ""
analyzeCStatement (CFor (Right d) _ _ s _) = ""
analyzeCStatement (CGoto _ _) = ""
analyzeCStatement (CGotoPtr e _) = ""
analyzeCStatement (CCont _) = ""
analyzeCStatement (CBreak _) = ""
analyzeCStatement (CReturn Nothing _) = ""
analyzeCStatement (CReturn (Just e) _) = ""
analyzeCStatement (CAsm _ _) = ""

showFile Nothing = "<undefined file>"
showFile (Just f) = f

isPtr attrs | 0 < length [x | x@(CPtrDeclr _ _) <- attrs] = True
            | otherwise = False


analyzeCCompound (CBlockStmt a) = analyzeCStatement a
analyzeCCompound (CBlockDecl a) = analyzeCDecl a
analyzeCCompound (CNestedFunDef a) = analyzeFunDef a
