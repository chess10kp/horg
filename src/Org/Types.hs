module Org.Types where

import Data.Text (Text)

data Document = Document [Block] deriving (Show, Eq)

data Block
  = Headline Int Text [Block]
  | Paragraph [Inline]
  | List ListType [ListItem]
  | CodeBlock Text Text
  | Keyword Text Text
  | PropertyDrawer [(Text, Text)]
  | Table [TableRow]
  | Comment Text
  deriving (Show, Eq)

data Inline
  = Plain Text
  | Bold [Inline]
  | Italic [Inline]
  | Underline [Inline]
  | Code Text
  | Verbatim Text
  | Strike [Inline]
  | Link Text (Maybe Text)
  deriving (Show, Eq)

data ListType = Unordered | Ordered deriving (Show, Eq)

data ListItem = ListItem [Inline] [Block] deriving (Show, Eq)

data TableRow = TableRow [Text] deriving (Show, Eq)