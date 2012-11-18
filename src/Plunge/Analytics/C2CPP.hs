{-# LANGUAGE ViewPatterns #-}
module Plunge.Analytics.C2CPP
  ( lineAssociations
  ) where

import Data.List
import Plunge.Types.PreprocessorOutput

-- Fill in any gaps left by making associations out of CPP spans.
lineAssociations :: [Section] -> [LineAssociation]
lineAssociations ss = concat $ snd $ mapAccumL fillGap 1 assocs
  where
    fillGap :: LineNumber -> LineAssociation -> (LineNumber, [LineAssociation])
    fillGap n (LineAssociation Nothing Nothing) = (n, [])
    fillGap n a@(LineAssociation Nothing (Just _)) = (n, [a])
    fillGap n a@(LineAssociation (Just clr) Nothing) = (toLine clr, [a])
    fillGap n a@(LineAssociation (Just clr) (Just _)) | n < cFrom = (cTo, [gap, a])
                                                      | n == cFrom = (cTo, [a])
                                                      | otherwise = (n, []) -- n > cFrom
      where cTo = toLine clr
            cFrom = fromLine clr
            gap = LineAssociation (Just $ LineRange n cFrom) Nothing

    assocs = sectionLineAssociations ss

sectionLineAssociations :: [Section] -> [LineAssociation]
sectionLineAssociations ss = map lineAssoc ss
  where
    lineAssoc (Block bls sl lr) =
      LineAssociation { cppRange = Just lr
                      , cRange = Just $ LineRange sl (sl + (length bls))
                      }
    lineAssoc (MiscDirective _ lr) =
      LineAssociation { cppRange = Just lr
                      , cRange = Nothing
                      }
    lineAssoc (Expansion _ (CppDirective stop _ _) n _ lr) =
      LineAssociation { cppRange = Just lr
                      , cRange = Just $ LineRange n stop
                      }
