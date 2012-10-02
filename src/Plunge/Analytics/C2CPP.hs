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

pairSpan :: ([Span], [CLine]) -> [(Maybe Span, [(CLine, Int)])]
pairSpan (ss, ls) = unfoldr pairer (ss, zip ls lineNums)
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
