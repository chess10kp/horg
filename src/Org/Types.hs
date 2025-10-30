module Org.Types where

import Data.Text (Text)

data Document = Document [Block] deriving (Show, Eq)

data TodoState = Todo | Done | Other Text deriving (Show, Eq)

data Progress = Progress Int Int deriving (Show, Eq)

data Block
  = Headline Int (Maybe TodoState) Text (Maybe Progress) [Block]
  | Paragraph [Inline]
  | List ListType [ListItem]
  | CodeBlock Text Text
  | Keyword Text Text
  | PropertyDrawer [(Text, Text)]
  | LogbookDrawer [LogbookEntry]
  | Timestamp Text
  | Table [TableRow]
  | Comment Text
  | EmptyLine
  deriving (Show, Eq)

data LogbookEntry
  = ClockEntry Text Text Text  -- start time, end time, duration
  | StateChange Text Text Text  -- from state, to state, timestamp
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