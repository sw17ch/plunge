module Plunge.Types.PreprocessorOutput
  ( Section(..)
  , CppDirective(..)
  , DirectiveFlag(..)
  , C2Cpp
  , Span(..)
  , NuSpanPair(..)
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
      , blockCppSpan    :: (FromLine, ToLine)
      }
  | MiscDirective
      { directive            :: CppDirective
      , startLine            :: LineNumber
      , miscDirectiveCppSpan :: (FromLine, ToLine)
      }
  | Expansion
      { enterDirective   :: CppDirective
      , returnDirective  :: CppDirective
      , startLine        :: LineNumber
      , sections         :: [Section]
      , expansionCppSpan :: (FromLine, ToLine)
      }
  deriving (Show)

data Span = Span { fromLine :: FromLine
                 , toLine   :: ToLine
                 , section  :: Section
                 } deriving (Show)

data NuSpanPair = NuSpanPair { cSpan   :: Maybe (FromLine, ToLine)
                             , cppSpan :: Maybe (FromLine, ToLine)
                             } deriving (Show)

type FromLine = Int
type ToLine   = Int

type LineNum  = Int
type SpanPair = (Maybe Span, [(CLine, LineNum)])


type CLine   = String
type CppLine = String
type Nesting = Int
type C2Cpp   = (Maybe CLine, Maybe (CppLine, Nesting))
