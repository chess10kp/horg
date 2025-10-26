module Org.Json where

import Data.Aeson (ToJSON(toJSON), object, (.=), Value(String))
import Data.Aeson.Key (fromText)
import Data.Text (Text, pack)
import Org.Types

instance ToJSON Document where
  toJSON (Document blocks) = object [fromText (pack "document") .= blocks]

instance ToJSON Block where
  toJSON (Headline level title content) = object [fromText (pack "type") .= pack "headline", fromText (pack "level") .= level, fromText (pack "title") .= title, fromText (pack "content") .= content]
  toJSON (Paragraph inlines) = object [fromText (pack "type") .= pack "paragraph", fromText (pack "content") .= inlines]
  toJSON (List typ items) = object [fromText (pack "type") .= pack "list", fromText (pack "listType") .= show typ, fromText (pack "items") .= items]
  toJSON (CodeBlock lang code) = object [fromText (pack "type") .= pack "codeBlock", fromText (pack "language") .= lang, fromText (pack "code") .= code]
  toJSON (Keyword key value) = object [fromText (pack "type") .= pack "keyword", fromText (pack "key") .= key, fromText (pack "value") .= value]
  toJSON (PropertyDrawer props) = object [fromText (pack "type") .= pack "propertyDrawer", fromText (pack "properties") .= props]
  toJSON (Table rows) = object [fromText (pack "type") .= pack "table", fromText (pack "rows") .= rows]
  toJSON (Comment text) = object [fromText (pack "type") .= pack "comment", fromText (pack "text") .= text]

instance ToJSON Inline where
  toJSON (Plain text) = object [fromText (pack "type") .= pack "plain", fromText (pack "text") .= text]
  toJSON (Bold inlines) = object [fromText (pack "type") .= pack "bold", fromText (pack "content") .= inlines]
  toJSON (Italic inlines) = object [fromText (pack "type") .= pack "italic", fromText (pack "content") .= inlines]
  toJSON (Underline inlines) = object [fromText (pack "type") .= pack "underline", fromText (pack "content") .= inlines]
  toJSON (Code text) = object [fromText (pack "type") .= fromText (pack "code"), fromText (pack "text") .= text]
  toJSON (Verbatim text) = object [fromText (pack "type") .= pack "verbatim", fromText (pack "text") .= text]
  toJSON (Strike inlines) = object [fromText (pack "type") .= pack "strike", fromText (pack "content") .= inlines]
  toJSON (Link url desc) = object [fromText (pack "type") .= pack "link", fromText (pack "url") .= url, fromText (pack "description") .= desc]

instance ToJSON ListType where
  toJSON Unordered = String (pack "unordered")
  toJSON Ordered = String (pack "ordered")

instance ToJSON ListItem where
  toJSON (ListItem content blocks) = object [fromText (pack "content") .= content, fromText (pack "blocks") .= blocks]

instance ToJSON TableRow where
  toJSON (TableRow cells) = object [fromText (pack "cells") .= cells]