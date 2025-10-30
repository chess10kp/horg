module Org.Json where

import Data.Aeson (ToJSON(toJSON), object, (.=), Value(String))
import Data.Aeson.Key (fromText)
import Data.Text (Text, pack)
import Org.Types

instance ToJSON TodoState where
  toJSON Todo = String (pack "TODO")
  toJSON Done = String (pack "DONE")
  toJSON (Other state) = String state

instance ToJSON Progress where
  toJSON (Progress done total) = object [fromText (pack "done") .= done, fromText (pack "total") .= total]

instance ToJSON Document where
  toJSON (Document blocks) = object [fromText (pack "document") .= blocks]

instance ToJSON Block where
  toJSON (Headline level todoState title progress content) = object [fromText (pack "type") .= pack "headline", fromText (pack "level") .= level, fromText (pack "todoState") .= todoState, fromText (pack "title") .= title, fromText (pack "progress") .= progress, fromText (pack "content") .= content]
  toJSON (Paragraph inlines) = object [fromText (pack "type") .= pack "paragraph", fromText (pack "content") .= inlines]
  toJSON (List typ items) = object [fromText (pack "type") .= pack "list", fromText (pack "listType") .= show typ, fromText (pack "items") .= items]
  toJSON (CodeBlock lang code) = object [fromText (pack "type") .= pack "codeBlock", fromText (pack "language") .= lang, fromText (pack "code") .= code]
  toJSON (Keyword key value) = object [fromText (pack "type") .= pack "keyword", fromText (pack "key") .= key, fromText (pack "value") .= value]
  toJSON (PropertyDrawer props) = object [fromText (pack "type") .= pack "propertyDrawer", fromText (pack "properties") .= props]
  toJSON (LogbookDrawer entries) = object [fromText (pack "type") .= pack "logbookDrawer", fromText (pack "entries") .= entries]
  toJSON (Timestamp text) = object [fromText (pack "type") .= pack "timestamp", fromText (pack "text") .= text]
  toJSON (Table rows) = object [fromText (pack "type") .= pack "table", fromText (pack "rows") .= rows]
  toJSON (Comment text) = object [fromText (pack "type") .= pack "comment", fromText (pack "text") .= text]
  toJSON EmptyLine = object [fromText (pack "type") .= pack "emptyLine"]

instance ToJSON Inline where
  toJSON (Plain text) = object [fromText (pack "type") .= pack "plain", fromText (pack "text") .= text]
  toJSON (Bold inlines) = object [fromText (pack "type") .= pack "bold", fromText (pack "content") .= inlines]
  toJSON (Italic inlines) = object [fromText (pack "type") .= pack "italic", fromText (pack "content") .= inlines]
  toJSON (Underline inlines) = object [fromText (pack "type") .= pack "underline", fromText (pack "content") .= inlines]
  toJSON (Code text) = object [fromText (pack "type") .= pack "code", fromText (pack "text") .= text]
  toJSON (Verbatim text) = object [fromText (pack "type") .= pack "verbatim", fromText (pack "text") .= text]
  toJSON (Strike inlines) = object [fromText (pack "type") .= pack "strike", fromText (pack "content") .= inlines]
  toJSON (Link url desc) = object [fromText (pack "type") .= pack "link", fromText (pack "url") .= url, fromText (pack "description") .= desc]

instance ToJSON ListType where
  toJSON Unordered = String (pack "unordered")
  toJSON Ordered = String (pack "ordered")

instance ToJSON ListItem where
  toJSON (ListItem content blocks) = object [fromText (pack "content") .= content, fromText (pack "blocks") .= blocks]

instance ToJSON LogbookEntry where
  toJSON (ClockEntry start end duration) = object [fromText (pack "type") .= pack "clock", fromText (pack "start") .= start, fromText (pack "end") .= end, fromText (pack "duration") .= duration]
  toJSON (StateChange fromState toState timestamp) = object [fromText (pack "type") .= pack "stateChange", fromText (pack "from") .= fromState, fromText (pack "to") .= toState, fromText (pack "timestamp") .= timestamp]

instance ToJSON TableRow where
  toJSON (TableRow cells) = object [fromText (pack "cells") .= cells]