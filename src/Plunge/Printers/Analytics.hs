module Plunge.Printers.Analytics
  ( renderAssociation
  ) where

import Plunge.Types.PreprocessorOutput
import Data.List

clampWidth :: Int
clampWidth = 50

clamp :: Int -> Int
clamp x = min x clampWidth

lrToSubListSpan :: LineRange -> (FromLine, ToLine)
lrToSubListSpan (LineRange fl tl) = (fl - 1, tl - 1)

rangeSize :: Maybe LineRange -> Int
rangeSize Nothing = 0
rangeSize (Just (LineRange fl tl)) = tl - fl

subList :: (Int, Int) -> [a] -> [a]
subList (begin, end) ls = take (end - begin) . drop begin $ ls

renderAssociation :: [LineAssociation] -> [CLine] -> [CppLine] -> String
renderAssociation las cls cppls =
  concat $ intersperse divider $ map renderAssoc las
  where
    longestCLine = maximum $ map length cls
    longestCppLine = maximum $ map length cppls
    sepString = " ||| "
    lineWidth = length sepString + clamp longestCLine + clamp longestCppLine
    divider = (take lineWidth $ repeat '-') ++ "\n"
    renderAssoc la =
      let cSize = rangeSize $ cRange la
          cppSize = rangeSize $ cppRange la
          assocSize = max cSize cppSize
          cLines = case cRange la of
                     (Just lr) -> subList (lrToSubListSpan lr) cls
                     Nothing -> []
          cppLines = case cppRange la of
                       (Just lr) -> subList (lrToSubListSpan lr) cppls
                       Nothing -> []
          paddedCLines   = padLines cLines   (clamp longestCLine)   assocSize ' ' '.'
          paddedCppLines = padLines cppLines (clamp longestCppLine) assocSize ' ' '.'
      in unlines $ zipWith (\c p -> c ++ sepString ++ p) paddedCLines paddedCppLines

padLines :: [[a]] -> Int -> Int -> a -> a -> [[a]]
padLines ls width len linePadChar emptyLineChar =
  let emptyPadLine = take width $ repeat emptyLineChar
      linePadding = repeat linePadChar
      padder l = take width $ l ++ linePadding
  in take len $ (map padder ls) ++ (repeat emptyPadLine)
