module Plunge.Types.PreprocessorOutput
  ( LineNumber
  , DirectiveFlag(..)
  , CppDirective(..)
  , Section(..)
  ) where

type LineNumber = Int
data DirectiveFlag = EnterFile | ReturnFile | SystemHeader | ExternC
  deriving (Show, Ord, Eq)
data CppDirective = CppDirective LineNumber FilePath [DirectiveFlag]
  deriving (Show)

data Section
  = Block [String]
  | MiscDirective
      { directive :: CppDirective
      }
  | Expansion
      { enterDirective   :: CppDirective
      , returnDirective  :: CppDirective
      , startLine        :: LineNumber
      , sections         :: [Section]
      }
  deriving (Show)
