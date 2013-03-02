module Plunge.Printers.Analytics
  ( renderAssociation
  ) where

import Plunge.Types.PreprocessorOutput
import Data.List
import Data.Maybe

lrToSubListSpan :: LineRange -> (FromLine, ToLine)
lrToSubListSpan (LineRange fl tl) = (fl - 1, tl - 1)

rangeSize :: Maybe LineRange -> Int
rangeSize Nothing = 0
rangeSize (Just (LineRange fl tl)) = tl - fl

subList :: (Int, Int) -> [a] -> [a]
subList (begin, end) ls = take (end - begin) . drop begin $ ls

renderAssociation :: [LineAssociation]
                  -> [CLine]
                  -> [CppLine]
                  -> String
                  -> String
                  -> Maybe Int
                  -> String
                  -> String
                  -> String
renderAssociation las cls cppls
                  _linePadder
                  _emptyLine
                  _maxWidth
                  _verticalSep
                  _horizSep
  = intercalate divider $ map renderAssoc las
  where
    longestCLine = maximum $ map length cls
    longestCppLine = maximum $ map length cppls
    sepString = _verticalSep
    maxLineLen = fromMaybe maxBound _maxWidth
    clamp = min maxLineLen
    lineWidth = length sepString + clamp longestCLine + clamp longestCppLine
    divider = take lineWidth (cycle _horizSep) ++ "\n"
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
          paddedCLines   = padLines cLines   (clamp longestCLine)   assocSize _linePadder _emptyLine
          paddedCppLines = padLines cppLines (clamp longestCppLine) assocSize _linePadder _emptyLine
      in unlines $ zipWith (\c p -> c ++ sepString ++ p) paddedCLines paddedCppLines

padLines :: [String] -> Int -> Int -> String -> String -> [String]
padLines ls width len linePadStr emptyLineStr =
  let emptyPadLine = take width $ cycle emptyLineStr
      padder l = take width $ l ++ cycle linePadStr
  in take len $ map padder ls ++ repeat emptyPadLine
