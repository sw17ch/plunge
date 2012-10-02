module Plunge.Printers.Analytics
  ( renderC2Cpp
  ) where

import Plunge.Types.PreprocessorOutput
import Text.PrettyPrint

-- import Text.PrettyPrint.HughesPJClass ((<+>), (<>), ($$), ($+$))

renderC2Cpp :: [C2Cpp] -> String
renderC2Cpp = render . formatC2Cpp

formatC2Cpp :: [C2Cpp] -> Doc
formatC2Cpp ls =
  let cLines = map (cLineToText . fst) ls
      cppLines = map (cppLineToText . snd) ls
      nested = map (nest (longestCLine + 3)) cppLines
  in vcat $ zipWith ($$) cLines nested
  where
    chomp = takeWhile (/= '\n')
    asterisks = repeat '*'
    longestCLine = maximum $ map (cLineLength . fst) ls
    cLineLength Nothing = 0
    cLineLength (Just l) = length l
    cLineToText (Just l) = text $ chomp l
    cLineToText _ = text $ take longestCLine asterisks
    longestCppLine = maximum $ map (cppLineLength . snd) ls
    cppLineLength Nothing = 0
    cppLineLength (Just (l, _)) = length l
    cppLineToText Nothing = text $ take longestCppLine asterisks
    cppLineToText (Just (l, _)) = text $ chomp l
