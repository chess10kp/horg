{-# LANGUAGE OverloadedStrings #-}

module Org.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Char (isSpace)
import Org.Types

type Parser = Parsec Void Text

parseDocument :: Parser Document
parseDocument = Document <$> many parseBlock <* eof

parseBlock :: Parser Block
parseBlock = choice
  [ try parseHeadline
  , try parseList
  , try parseCodeBlock
  , try parseKeyword
  , try parsePropertyDrawer
  , try parseTable
  , try parseComment
  , parseParagraph
  ]

parseHeadline :: Parser Block
parseHeadline = do
  level <- length <$> some (char '*')
  space1
  title <- takeWhile1P Nothing (/= '\n')
  newline
  content <- many parseBlock
  return $ Headline level title content

parseParagraph :: Parser Block
parseParagraph = do
  content <- some parseInline <* newline
  return $ Paragraph content

parseList :: Parser Block
parseList = do
  listType <- (Unordered <$ char '-') <|> (Ordered <$ (decimal <* char '.'))
  space1
  items <- some parseListItem
  return $ List listType items

parseListItem :: Parser ListItem
parseListItem = do
  content <- some parseInline <* newline
  blocks <- many (try parseBlock)
  return $ ListItem content blocks

parseCodeBlock :: Parser Block
parseCodeBlock = do
  string "#+BEGIN_SRC"
  space1
  lang <- takeWhile1P Nothing (not . isSpace)
  newline
  code <- takeWhile1P Nothing (/= '\n') `sepBy` newline
  string "#+END_SRC"
  return $ CodeBlock lang (T.unlines code)

parseKeyword :: Parser Block
parseKeyword = do
  char '#'
  char '+'
  key <- takeWhile1P Nothing (/= ':')
  char ':'
  space
  value <- takeWhile1P Nothing (/= '\n')
  return $ Keyword key value

parsePropertyDrawer :: Parser Block
parsePropertyDrawer = do
  string ":PROPERTIES:"
  newline
  props <- many parseProperty
  string ":END:"
  return $ PropertyDrawer props

parseProperty :: Parser (Text, Text)
parseProperty = do
  char ':'
  key <- takeWhile1P Nothing (/= ':')
  char ':'
  space
  value <- takeWhile1P Nothing (/= '\n')
  newline
  return (key, value)

parseTable :: Parser Block
parseTable = do
  rows <- some parseTableRow
  return $ Table rows

parseTableRow :: Parser TableRow
parseTableRow = do
  char '|'
  cells <- many (takeWhile1P Nothing (/= '|') <* char '|')
  newline
  return $ TableRow cells

parseComment :: Parser Block
parseComment = do
  char '#'
  text <- takeWhile1P Nothing (/= '\n')
  return $ Comment text

parseInline :: Parser Inline
parseInline = choice
  [ try parseBold
  , try parseItalic
  , try parseUnderline
  , try parseCode
  , try parseVerbatim
  , try parseStrike
  , try parseLink
  , parsePlain
  ]

parseBold :: Parser Inline
parseBold = do
  char '*'
  content <- some parseInline
  char '*'
  return $ Bold content

parseItalic :: Parser Inline
parseItalic = do
  char '/'
  content <- some parseInline
  char '/'
  return $ Italic content

parseUnderline :: Parser Inline
parseUnderline = do
  char '_'
  content <- some parseInline
  char '_'
  return $ Underline content

parseCode :: Parser Inline
parseCode = do
  char '~'
  content <- takeWhile1P Nothing (/= '~')
  char '~'
  return $ Code content

parseVerbatim :: Parser Inline
parseVerbatim = do
  char '='
  content <- takeWhile1P Nothing (/= '=')
  char '='
  return $ Verbatim content

parseStrike :: Parser Inline
parseStrike = do
  char '+'
  content <- some parseInline
  char '+'
  return $ Strike content

parseLink :: Parser Inline
parseLink = do
  char '['
  char '['
  url <- takeWhile1P Nothing (/= ']')
  char ']'  -- close url part
  desc <- (char '[' *> takeWhile1P Nothing (/= ']') <* char ']') <|> pure ""
  char ']'
  return $ Link url (if T.null desc then Nothing else Just desc)

parsePlain :: Parser Inline
parsePlain = do
  text <- takeWhile1P Nothing (\c -> not (isSpace c) && c `notElem` ("*_/~=+[]" :: String))
  return $ Plain text