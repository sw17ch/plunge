module Plunge.Printers.Analytics
  ( renderC2Cpp
  ) where

import Plunge.Types.PreprocessorOutput

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint.HughesPJClass ((<+>), (<>), ($$), ($+$))

renderC2Cpp :: [C2Cpp] -> String
renderC2Cpp = formatC2Cpp -- PP.render . formatC2Cpp

formatC2Cpp :: [C2Cpp] -> String
formatC2Cpp ls = show $ map snd ls
