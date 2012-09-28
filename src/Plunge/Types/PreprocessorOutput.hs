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
  = Block
      { blockLines :: [String]
      , startLine  :: LineNumber
      }
  | MiscDirective
      { directive :: CppDirective
      , startLine :: LineNumber
      }
  | Expansion
      { enterDirective   :: CppDirective
      , returnDirective  :: CppDirective
      , startLine        :: LineNumber
      , sections         :: [Section]
      }
  deriving (Show)
