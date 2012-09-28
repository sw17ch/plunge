module Plunge.Analytics.C2CPP
  ( c2cpp
  , CData
  , CLine
  , CLines
  , SectionMapping
  ) where

import Plunge.Preprocessor
import Plunge.Types.PreprocessorOutput

type CData  = String
type CLine  = String
type CLines = [CLine]

type SectionMapping = ([CLine], Section)

c2cpp :: CData -> [Section] -> [(Int, Int, Section)]
c2cpp d ss = map mkSpan ss
  where
    mkSpan s@(Block _)         = (-1, -1, s)
    mkSpan s@(MiscDirective _) = (-1, -1, s)
    mkSpan s@(Expansion ed rd n _) =
      let (CppDirective start _ _) = ed
          (CppDirective stop  _ _) = rd
      in (n, stop, s)



{-
 - CSection -> CPPSection
 -
 -        becomes
 -
 - (Maybe CLine, CPPLine, Nesting)
 -    
 -}
