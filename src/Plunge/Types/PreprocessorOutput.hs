module Plunge.Types.PreprocessorOutput
  ( Section(..)
  , LineRange(..)
  , LineAssociation(..)
  , LineNumber
  , CppDirective(..)
  , DirectiveFlag(..)
  ) where

type LineNumber = Int
type FromLine = Int
type ToLine   = Int

data DirectiveFlag = EnterFile | ReturnFile | SystemHeader | ExternC
  deriving (Show, Ord, Eq)
data CppDirective = CppDirective LineNumber FilePath [DirectiveFlag]
  deriving (Show)

data LineRange = LineRange { fromLine :: FromLine
                           , toLine :: ToLine
                           } deriving (Show)

data Section
  = Block
      { blockLines  :: [String]
      , startLine   :: LineNumber
      , lineRange :: LineRange
      }
  | MiscDirective
      { directive   :: CppDirective
      , lineRange :: LineRange
      }
  | Expansion
      { enterDirective  :: CppDirective
      , returnDirective :: CppDirective
      , startLine       :: LineNumber
      , sections        :: [Section]
      , lineRange     :: LineRange
      }
  deriving (Show)

data LineAssociation = LineAssociation { cRange :: Maybe LineRange
                                       , cppRange :: Maybe LineRange
                                       } deriving (Show)

