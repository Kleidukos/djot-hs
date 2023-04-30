{-# LANGUAGE StrictData #-}

module Djot.AST where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Word (Word8)

newtype Target = Target Text
  deriving newtype (Eq, Ord, Show)

data Attribute = Attribute {getAttribute :: Map Text Text}
  deriving stock (Eq, Ord, Show)

data ListKind
  = Ordered
  | Unordered
  deriving stock (Eq, Ord, Show)

newtype ListItem = Item AST
  deriving stock (Eq, Ord, Show)

data AST
  = -- Inline syntax
    InlineLink {-# UNPACK #-} Target (Vector Attribute)
  | ReferenceLink (Maybe Target) (Vector Attribute)
  | Image {-# UNPACK #-} Target (Vector Attribute)
  | Autolink {-# UNPACK #-} Target (Vector Attribute)
  | Verbatim {-# UNPACK #-} Text (Vector Attribute)
  | Emphasis AST (Vector Attribute)
  | StrongEmphasis AST (Vector Attribute)
  | Highlight AST (Vector Attribute)
  | Superscript AST (Vector Attribute)
  | Subscript AST (Vector Attribute)
  | DoubleQuotes AST (Vector Attribute)
  | SingleQuote AST (Vector Attribute)
  | Escaped {-# UNPACK #-} Char (Vector Attribute)
  | -- | '…'
    Ellipsis {-# UNPACK #-} (Vector Attribute)
  | -- | '—'
    EmDash {-# UNPACK #-} (Vector Attribute)
  | -- | '–'
    EnDash {-# UNPACK #-} (Vector Attribute)
  | -- | '$' + verbatim
    InlineMath {-# UNPACK #-} Text (Vector Attribute)
  | -- | '$$' + verbatim
    DisplayMath {-# UNPACK #-} Text (Vector Attribute)
  | FootnoteReference {-# UNPACK #-} Text (Vector Attribute)
  | SoftLineBreak {-# UNPACK #-} (Vector Attribute)
  | HardLineBreak {-# UNPACK #-} (Vector Attribute)
  | Symbol {-# UNPACK #-} Text (Vector Attribute)
  | RawInline
      {-# UNPACK #-} Text
      -- ^ Content
      Text
      -- ^ Format
      (Vector Attribute)
  | Span
      Text
      -- ^ Content
      (Vector Attribute)
      -- ^ Attributes
  | Container Container
  deriving stock (Eq, Ord, Show)

data Container
  = Paragraph AST (Vector Attribute)
  | Header Word8 (Vector Attribute)
  | BlockQuote AST (Vector Attribute)
  | List ListKind (Vector ListItem) (Vector Attribute)
  | ListItem ListItem (Vector Attribute)
  | -- | Checked?
    TaskListItem
      Bool
  | DefinitionListItem
      Text
      -- ^ Term
      Text
      -- ^ Definition
  | CodeBlock AST (Vector Attribute)
  | ThematicBreak (Vector Attribute)
  | RawBlock AST (Vector Attribute)
  | Div AST (Vector Attribute)
  | PipeTable
  | ReferenceLinkDefinition Target (Vector Attribute)
  | Footnote {-# UNPACK #-} (Vector Attribute)
  deriving stock (Eq, Ord, Show)
