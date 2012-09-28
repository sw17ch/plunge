module Plunge.Printers.PreprocessorOutput
  ( renderOriginal
  ) where

import Plunge.Types.PreprocessorOutput

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint.HughesPJClass ((<+>), (<>))

renderOriginal :: Section -> String
renderOriginal s = PP.render . originalSection $ s

originalSection (Block ls) = PP.vcat $ map (PP.text . (takeWhile (/= '\n'))) ls
originalSection (MiscDirective d) = originalCppDirective d
originalSection (Expansion ed rd _ ss) = PP.vcat [ originalCppDirective ed
                                                 , PP.vcat $ map originalSection ss
                                                 , originalCppDirective rd
                                                 ]

originalCppDirective (CppDirective n p ds)
    = PP.char '#'
  <+> (PP.int n)
  <+> (PP.doubleQuotes $ PP.text p)
  <+> (PP.hsep $ map originalDirectiveFlag ds)

originalDirectiveFlag EnterFile    = PP.char '1'
originalDirectiveFlag ReturnFile   = PP.char '2'
originalDirectiveFlag SystemHeader = PP.char '3'
originalDirectiveFlag ExternC      = PP.char '4'
