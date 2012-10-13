module Plunge.Analytics.C2CPP
  ( spans
  , nuSpans
  , pairSpans
  , pairSpanLinesWithCLines
  ) where

import Data.List
import Plunge.Types.PreprocessorOutput
import Plunge.Printers.PreprocessorOutput

----------------------------------------------------------------------
nuSpans :: [Section] -> [NuSpan]
nuSpans ss = map mkSpan ss
  where
    mkSpan s@(Block ls sn)         = Span sn (sn + (length ls)) s
    mkSpan s@(MiscDirective _ sn)  = Span sn sn s
    mkSpan s@(Expansion _ rd n _)  =
      let (CppDirective stop  _ _) = rd
      in Span n stop s
----------------------------------------------------------------------

spans :: [Section] -> [Span]
spans ss = map mkSpan ss
  where
    mkSpan s@(Block ls sn _)         = Span sn (sn + (length ls)) s
    mkSpan s@(MiscDirective _ sn _)  = Span sn sn s
    mkSpan s@(Expansion _ rd n _ _)  =
      let (CppDirective stop _ _) = rd
      in Span n stop s

-- |Given an ordered list of spans, and an ordered list of lines, pairSpan
-- produces a list of (Maybe Span -> [(CLine, LineNum)]) mappings. When a span
-- has no associated lines from the original C file, it is paired with an empty
-- list. When a set of lines from the C file has no corresponding span from the
-- preprocessed text, it is paired with Nothing.
pairSpans :: [Span] -> [CLine] -> [SpanPair]
pairSpans cppSpans cLines = unfoldr pairer (cppSpans, zip cLines lineNums)
  where
    lineNums = [1..]
    pairer ([],   []) = Nothing -- We're done!
    pairer ([],   ls) = Just ((Nothing, ls), ([], [])) -- Only lines remain.
    pairer (s:ss, []) = Just ((Just s, []), (ss, [])) -- Only spans remain.
    -- We still have spans and lines. Consume the proper lines and spans.
    pairer (ss@(s:ss'), ls) | n < from  = let (ls',rest) = span (lineLessThan from) ls
                                          in Just ((Nothing, ls'), (ss, rest))
                            | otherwise = let (ls',rest) = span (lineLessThan to) ls
                                          in Just ((Just s, ls'), (ss', rest))
      where
        lineLessThan x (_,lineNum) = lineNum < x
        (Span from to _):_ = ss
        (_,n):_            = ls

pairSpanLinesWithCLines :: [SpanPair] -> [C2Cpp]
pairSpanLinesWithCLines sPairs = concatMap mkPairs sPairs

mkPairs :: SpanPair -> [C2Cpp]
mkPairs pair =
  case pair of
    (Nothing, [])      -> []
    (Nothing, ls)      -> zip (map (Just . fst) ls) nothings
    (Just (Span _ _ s), []) -> map (\(l,n) -> (Nothing, Just (l, n))) $ section2LineAndNesting s
    (Just (Span _ _ s), ls) ->
      let ls' = (map (Just . fst) ls) ++ nothings
          s'  = map Just (section2LineAndNesting s)
          s'' = s' ++ nothings
          numPairs = max (length ls) (length s')
      in take numPairs $ zip ls' s''
  where
    nothings = repeat Nothing

section2LineAndNesting :: Section -> [(CppLine, Nesting)]
section2LineAndNesting sec = s2lan 0 sec
  where showCppDir d = show $ originalCppDirective d
        s2lan n (Block ls _ _) = zip ls (repeat n)
        s2lan n (MiscDirective d _ _) = [(showCppDir d, n)]
        s2lan n (Expansion e r _ [] _) = [(showCppDir e,n), (showCppDir r,n)]
        s2lan n (Expansion e r _ ss _) =
          let rest = concatMap (s2lan (n + 1)) ss
              e' = (showCppDir e, n)
              r' = (showCppDir r, n)
          in [e'] ++ rest ++ [r']
