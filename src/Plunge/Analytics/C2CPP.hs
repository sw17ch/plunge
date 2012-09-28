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
    mkSpan (Block ss)
    mkSpans s = let start = 1
                    stop  = 1
                in  (start, stop, s)
      



{-
 - CSection -> CPPSection
 -
 -        becomes
 -
 - (Maybe CLine, CPPLine, Nesting)
 -    
 -}
