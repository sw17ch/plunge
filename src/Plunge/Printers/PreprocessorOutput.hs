module Plunge.Printers.PreprocessorOutput
  ( renderOriginal
  , originalCppDirective
  ) where

import Plunge.Types.PreprocessorOutput

import Text.PrettyPrint

renderOriginal :: Section -> String
renderOriginal s = render . originalSection $ s

originalSection :: Section -> Doc
originalSection (Block ls _ _) = vcat $ map (text . (takeWhile (/= '\n'))) ls
originalSection (MiscDirective d _) = originalCppDirective d
originalSection (Expansion ed rd _ ss _) = vcat [ originalCppDirective ed
                                                , vcat $ map originalSection ss
                                                , originalCppDirective rd
                                                ]

originalCppDirective :: CppDirective -> Doc
originalCppDirective (CppDirective n p ds)
    = char '#'
  <+> (int n)
  <+> (doubleQuotes $ text p)
  <+> (hsep $ map originalDirectiveFlag ds)

originalDirectiveFlag :: DirectiveFlag -> Doc
originalDirectiveFlag EnterFile    = char '1'
originalDirectiveFlag ReturnFile   = char '2'
originalDirectiveFlag SystemHeader = char '3'
originalDirectiveFlag ExternC      = char '4'
