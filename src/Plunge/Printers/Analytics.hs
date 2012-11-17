module Plunge.Printers.Analytics
  ( renderAssociation 
  ) where

import Plunge.Types.PreprocessorOutput
import Text.PrettyPrint

type CLine = String
type CppLine = String

lrToTuple (LineRange fl tl) = (fl, tl)

rangeSize Nothing = 0
rangeSize (Just (LineRange fl tl)) = tl - fl

subList :: (Int, Int) -> [a] -> [a]
subList (begin, end) lines = take (end - begin) . drop begin $ lines

renderAssociation :: [LineAssociation] -> [CLine] -> [CppLine] -> String
renderAssociation las cls cppls =
  concat $ map render las
  where
    longestCLine = maximum $ map length cls
    longestCppLine = maximum $ map length cppls
    emptyCLines = repeat $ take longestCLine $ repeat '~'
    emptyCppLines = repeat $ take longestCppLine $ repeat '~'
    render la =
      let cSize = rangeSize $ cRange la
          cppSize = rangeSize $ cppRange la
          assocSize = max cSize cppSize
          cLines = case cRange la of
                     (Just lr) -> subList (lrToTuple lr) cls
                     Nothing -> []
          cppLines = case cppRange la of
                       (Just lr) -> subList (lrToTuple lr) cppls
                       Nothing -> []
          assoc = zip (take assocSize (cLines ++ emptyCLines)) (take assocSize (cppLines ++ emptyCppLines))
      in unlines $ map (\(c, p) -> c ++ " ||| " ++ p) assoc
