module Plunge.Analytics.C2CPP
  ( spans
  , pairSpan
  ) where

import Data.List
import Plunge.Types.PreprocessorOutput

type FromLine = Int
type ToLine = Int

type Span = (FromLine, ToLine, Section)

spans :: [Section] -> [Span]
spans ss = map mkSpan ss
  where
    mkSpan s@(Block ls sn)         = (sn, sn + (length ls), s)
    mkSpan s@(MiscDirective _ sn)  = (sn, sn, s)
    mkSpan s@(Expansion _ rd n _)  =
      let (CppDirective stop  _ _) = rd
      in (n, stop, s)

type CLine = String
type CppLine = String

-- |Given an ordered list of spans, and an ordered list of lines, pairSpan
-- produces a list of (Maybe Span -> [(CLine, LineNum)]) mappings. When a span
-- has no associated lines from the original C file, it is paired with an empty
-- list. When a set of lines from the C file has no corresponding span from the
-- preprocessed text, it is paired with Nothing.
pairSpan :: [Span] -> [CLine] -> [(Maybe Span, [(CLine, Int)])]
pairSpan cppSpans cLines = unfoldr pairer (cppSpans, zip cLines lineNums)
  where
    lineNums = [1..]
    pairer :: ([Span], [(CLine, Int)]) -> Maybe ((Maybe Span, [(CLine,Int)]), ([Span], [(CLine, Int)]))
    pairer ([],   []) = Nothing
    pairer ([],   ls) = Just ((Nothing, ls), ([], []))
    pairer (s:ss, []) = Just ((Just s, []), (ss, []))
    pairer (ss@(s:ss'), ls) | n < from  = let (ls',rest) = span (\(_,n') -> n' < from) ls
                                          in Just ((Nothing, ls'), (ss, rest))
                            | otherwise = let (ls',rest) = span (\(_,n') -> n' < to) ls
                                          in Just ((Just s, ls'), (ss', rest))
      where
        (from, to, _):_ = ss
        (_,n):_         = ls


type Nesting = Int
type CppMapping = (Maybe CLine, Nesting, Maybe CppLine)

{-
 - CSection -> CPPSection
 -
 -        becomes
 -
 - (Maybe CLine, CPPLine, Nesting)
 -
 -}
