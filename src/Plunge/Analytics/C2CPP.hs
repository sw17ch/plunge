module Plunge.Analytics.C2CPP
  ( spans
  , CData
  , CLine
  , CLines
  , SectionMapping
  ) where

import Plunge.Types.PreprocessorOutput

type CData  = String
type CLine  = String
type CLines = [CLine]

type SectionMapping = ([CLine], Section)

spans :: [Section] -> [(Int, Int, Section)]
spans ss = map mkSpan ss
  where
    mkSpan s@(Block ls sn)         = (sn, sn + (length ls), s)
    mkSpan s@(MiscDirective _ sn)  = (sn, sn, s)
    mkSpan s@(Expansion _ rd n _)  =
      let (CppDirective stop  _ _) = rd
      in (n, stop, s)

{-
 - CSection -> CPPSection
 -
 -        becomes
 -
 - (Maybe CLine, CPPLine, Nesting)
 -    
 -}
