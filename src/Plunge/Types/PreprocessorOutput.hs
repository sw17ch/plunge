module Plunge.Types.PreprocessorOutput
  ( Section(..)
  , CppDirective(..)
  , DirectiveFlag(..)
  , C2Cpp
  , Span(..)
  , CLine
  , SpanPair
  , CppLine
  , Nesting
  , LineNumber
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

data Span = Span { fromLine :: FromLine
                 , toLine   :: ToLine
                 , section  :: Section
                 } deriving (Show)
type FromLine = Int
type ToLine   = Int

type LineNum  = Int
type SpanPair = (Maybe Span, [(CLine, LineNum)])

type CLine   = String
type CppLine = String
type Nesting = Int
type C2Cpp   = (Maybe CLine, Maybe (CppLine, Nesting))
