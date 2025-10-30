{-# LANGUAGE OverloadedStrings #-}

module Org.Parser
  ( parseDocument
  , parseBlock
  , parseHeadline
  , parseHeadlineContent
  , parseInline
  , parseSpace
  , parseList
  , parseKeyword
  , parseComment
  , parseNonListBlock
  , parseParagraph
  , parseFirstListItem
  , parseIndentedList
  , parsePropertyDrawer
  , parseProperty
  , parseLogbookDrawer
  , parseClockEntry
  , parseStateChange
  ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (digitChar)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Org.Types (TodoState(..), Progress(..))
import Data.Void (Void)
import Data.Char (isSpace)
import Org.Types

type Parser = Parsec Void Text

parseDocument :: Parser Document
parseDocument = Document <$> many (parseBlock 0)

parseTitle :: Text -> (Maybe TodoState, Text)
parseTitle title = case T.words title of
  ("TODO":rest) -> (Just Todo, T.unwords rest)
  ("DONE":rest) -> (Just Done, T.unwords rest)
  (other:rest) | T.toUpper other `elem` ["TODO", "DONE", "WAIT"] -> (Just (Other other), T.unwords rest)
  _ -> (Nothing, title)

calculateProgress :: [Block] -> Maybe Progress
calculateProgress blocks = case countTodoHeadlines blocks of
  (0, 0) -> Nothing
  (done, total) -> Just (Progress done total)

countTodoHeadlines :: [Block] -> (Int, Int)
countTodoHeadlines = foldl count (0, 0)
  where
    count acc (Headline _ (Just Todo) _ progress _) = 
      let (doneSub, totalSub) = case progress of
            Just (Progress d t) -> (d, t)
            Nothing -> (0, 0)
          -- If a TODO task has more DONE than TODO subtasks, count it as DONE
          isEffectivelyDone = doneSub > 0 && doneSub >= (totalSub - doneSub)
      in if isEffectivelyDone 
         then (fst acc + 1, snd acc + 1) 
         else (fst acc, snd acc + 1)
    count acc (Headline _ (Just Done) _ _ _) = (fst acc + 1, snd acc + 1)
    count acc (Headline _ (Just (Other _)) _ _ _) = (fst acc, snd acc + 1)
    count acc _ = acc

parseBlock :: Int -> Parser Block
parseBlock minLevel = choice
  [ try (parseEmptyLine minLevel)
  , try (parseHeadline minLevel)
  , try (parsePropertyDrawer minLevel)  -- Try property drawer before paragraph
  , try (parseLogbookDrawer minLevel)   -- Try logbook drawer before paragraph
  , try (parseTimestamp)            -- Try timestamp before paragraph
  , try (parseList minLevel)
  , try (parseCodeBlock minLevel)
  , try (parseKeyword minLevel)
  , try (parseTable minLevel)
  , try (parseComment minLevel)
  , parseParagraph minLevel
  ]

parseNonListBlock :: Int -> Parser Block
parseNonListBlock minLevel = choice
  [ try (parseEmptyLine minLevel)
  , try (parseHeadline minLevel)
  , try (parseCodeBlock minLevel)
  , try (parseKeyword minLevel)
  , try (parsePropertyDrawer minLevel)
  , try (parseLogbookDrawer minLevel)
  , try (parseTable minLevel)
  , try (parseComment minLevel)
  , parseParagraph minLevel
  ]

parseHeadline :: Int -> Parser Block
parseHeadline minLevel = do
  level <- length <$> some (char '*')
  if level <= minLevel then fail "Lower or equal level headline"
  else do
    space1
    titleLine <- takeWhile1P Nothing (/= '\n')
    newline
    content <- many (parseHeadlineContent level)
    let (todoState, title) = parseTitle titleLine
        progress = calculateProgress content
    return $ Headline level todoState title progress content

parseHeadlineContent :: Int -> Parser Block
parseHeadlineContent level = choice
  [ try (parseEmptyLine level)
  , try (parseHeadline level)  -- Only parse deeper headlines
  , try (parsePropertyDrawer level)
  , try (parseLogbookDrawer level)
  , try (parseTimestamp)
  , try (parseList level)
  , try (parseCodeBlock level)
  , try (parseKeyword level)
  , try (parseTable level)
  , try (parseComment level)
  , parseParagraph level
  ]

parseParagraph :: Int -> Parser Block
parseParagraph _ = do
  -- Don't start with list markers, property drawers, logbook drawers, or timestamps
  notFollowedBy ((char '-' <|> digitChar) *> space1)
  notFollowedBy (string (pack ":PROPERTIES:"))
  notFollowedBy (string (pack ":LOGBOOK:"))
  notFollowedBy (string (pack ":END:"))
  notFollowedBy (string (pack "CLOCK:"))
  notFollowedBy (string (pack "- State"))
  notFollowedBy (char '<')
  notFollowedBy (char '#')
  content <- some (try parseInline) <* optional newline
  return $ Paragraph content

parseList :: Int -> Parser Block
parseList minLevel = do
  -- Look ahead to determine list type
  (listType, firstItem) <- parseFirstListItem
  restItems <- many (try (parseNextListItem listType))
  return $ List listType (firstItem : restItems)

parseFirstListItem :: Parser (ListType, ListItem)
parseFirstListItem = do
  listType <- (try (char '-' *> space1 *> pure Unordered)) <|> (try (decimal *> char '.' *> space1 *> pure Ordered))
  content <- some (try parseInline) <* optional newline
  blocks <- many (try (parseListItemContent))
  return (listType, ListItem content blocks)

parseNextListItem :: ListType -> Parser ListItem
parseNextListItem listType = do
  -- Parse the same list type marker
  case listType of
    Unordered -> char '-' *> space1
    Ordered -> decimal *> char '.' *> space1
  content <- some (try parseInline) <* optional newline
  blocks <- many (try (parseListItemContent))
  return $ ListItem content blocks

parseListItemContent :: Parser Block
parseListItemContent = do
  choice
    [ try (parseIndentedList)  -- Nested list (must be indented)
    , try (parseNonListItemBlock)  -- Other blocks that aren't list items
    , parseParagraph 0  -- Fallback to paragraph
    ]

parseNonListItemBlock :: Parser Block
parseNonListItemBlock = do
  -- Don't allow new list items at the same level
  notFollowedBy (try ((char '-' <|> digitChar) *> space1))
  choice
    [ try (parseEmptyLine 0)
    , try (parseHeadline 0)
    , try (parseCodeBlock 0)
    , try (parseKeyword 0)
    , try (parsePropertyDrawer 0)
    , try (parseLogbookDrawer 0)
    , try (parseTable 0)
    , try (parseComment 0)
    , parseParagraph 0  -- Paragraph without list marker check
    ]

parseIndentedList :: Parser Block
parseIndentedList = do
  -- Look for indentation (at least 2 spaces)
  count 2 (char ' ')
  parseIndentedListItems

parseIndentedListItems :: Parser Block
parseIndentedListItems = do
  items <- parseIndentedListItems'
  return $ List Unordered items

parseIndentedListItems' :: Parser [ListItem]
parseIndentedListItems' = do
  firstItem <- parseIndentedListItem
  restItems <- many (try parseNextIndentedListItem)
  return (firstItem : restItems)

parseIndentedListItem :: Parser ListItem
parseIndentedListItem = do
  -- Parse list item marker (no leading spaces now, since we consumed them)
  (char '-' *> space1 *> pure ()) <|> (decimal *> char '.' *> space1 *> pure ())
  content <- some (try parseInline) <* optional newline
  -- Parse nested content (must be even more indented)
  blocks <- many (try (parseMoreIndentedContent))
  return $ ListItem content blocks

parseNextIndentedListItem :: Parser ListItem
parseNextIndentedListItem = do
  -- Look for the 2-space indentation for next item
  count 2 (char ' ')
  -- Parse list item marker
  (char '-' *> space1 *> pure ()) <|> (decimal *> char '.' *> space1 *> pure ())
  content <- some (try parseInline) <* optional newline
  -- Parse nested content (must be even more indented)
  blocks <- many (try (parseMoreIndentedContent))
  return $ ListItem content blocks

parseMoreIndentedContent :: Parser Block
parseMoreIndentedContent = do
  -- Must be even more indented (at least 4 spaces total)
  count 4 (char ' ')
  choice
    [ try (parseIndentedList)  -- Even more nested list
    , try (parseNonListBlock 0)  -- Other blocks
    , parseParagraph 0  -- Indented paragraph
    ]

parseCodeBlock :: Int -> Parser Block
parseCodeBlock _ = do
  string (pack "#+BEGIN_SRC")
  space1
  lang <- takeWhile1P Nothing (not . isSpace)
  newline
  codeLines <- many (takeWhileP Nothing (/= '\n') <* newline)
  string (pack "#+END_SRC")
  optional newline
  return $ CodeBlock lang (T.unlines codeLines)

parseKeyword :: Int -> Parser Block
parseKeyword _ = do
  char '#'
  char '+'
  key <- takeWhile1P Nothing (/= ':')
  char ':'
  space
  value <- takeWhile1P Nothing (/= '\n')
  optional newline
  return $ Keyword key value

parsePropertyDrawer :: Int -> Parser Block
parsePropertyDrawer _ = do
  string (pack ":PROPERTIES:")
  newline
  props <- manyTill parseProperty (string (pack ":END:"))
  optional newline
  return $ PropertyDrawer props

parseProperty :: Parser (Text, Text)
parseProperty = do
  char ':'
  key <- takeWhile1P Nothing (/= ':')
  char ':'
  optional space
  value <- takeWhileP Nothing (/= '\n')
  newline
  return (key, T.strip value)

parseTable :: Int -> Parser Block
parseTable _ = do
  rows <- some (try parseTableRow)
  notFollowedBy (char '|')  -- Ensure we're at table end
  return $ Table rows

parseTableRow :: Parser TableRow
parseTableRow = do
  char '|'
  cells <- many (takeWhileP Nothing (/= '|') <* char '|')
  newline
  return $ TableRow (map T.strip cells)

parseComment :: Int -> Parser Block
parseComment _ = do
  char '#'
  text <- takeWhile1P Nothing (/= '\n')
  return $ Comment text

parseEmptyLine :: Int -> Parser Block
parseEmptyLine _ = do
  newline
  return EmptyLine

parseInline :: Parser Inline
parseInline = choice
  [ try parseBold
  , try parseItalic
  , try parseUnderline
  , try parseCode
  , try parseVerbatim
  , try parseStrike
  , try parseLink
  , parseSpace
  , parsePlain
  ]

parseBold :: Parser Inline
parseBold = do
  char '*'
  content <- manyTill (try parseInline) (char '*')
  return $ Bold content

parseItalic :: Parser Inline
parseItalic = do
  char '/'
  content <- manyTill (try parseInline) (char '/')
  return $ Italic content

parseUnderline :: Parser Inline
parseUnderline = do
  char '_'
  content <- manyTill (try parseInline) (char '_')
  return $ Underline content

parseCode :: Parser Inline
parseCode = do
  char '='
  content <- takeWhile1P Nothing (/= '=')
  char '='
  return $ Code content

parseVerbatim :: Parser Inline
parseVerbatim = do
  char '~'
  content <- takeWhile1P Nothing (/= '~')
  char '~'
  return $ Verbatim content

parseStrike :: Parser Inline
parseStrike = do
  char '+'
  content <- manyTill (try parseInline) (char '+')
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

parseSpace :: Parser Inline
parseSpace = do
  char ' '
  return $ Plain " "

parseLogbookDrawer :: Int -> Parser Block
parseLogbookDrawer _ = do
  string (pack ":LOGBOOK:")
  newline
  entries <- parseLogbookEntries
  string (pack ":END:")
  optional newline
  return $ LogbookDrawer entries

parseLogbookEntries :: Parser [LogbookEntry]
parseLogbookEntries = manyTill parseLogbookEntry (string (pack ":END:"))

parseLogbookEntry :: Parser LogbookEntry
parseLogbookEntry = choice
  [ try parseClockEntry
  , try parseStateChange
  ]

parseClockEntry :: Parser LogbookEntry
parseClockEntry = do
  string (pack "CLOCK: ")
  start <- takeWhile1P Nothing (/= ']')
  char ']'
  string (pack "--[")
  end <- takeWhile1P Nothing (/= ']')
  char ']'
  string (pack " => ")
  duration <- takeWhileP Nothing (/= '\n')
  optional newline
  return $ ClockEntry (start `T.append` pack "]") (pack "[" `T.append` end `T.append` pack "]") duration

parseStateChange :: Parser LogbookEntry
parseStateChange = do
  string (pack "- State \"")
  fromState <- takeWhile1P Nothing (/= '"')
  string (pack "\"")
  some space
  string (pack "from")
  some space
  string (pack "\"")
  toState <- takeWhile1P Nothing (/= '"')
  string (pack "\"")
  some space
  string (pack "[")
  timestamp <- takeWhile1P Nothing (/= ']')
  string (pack "]")
  optional newline
  return $ StateChange fromState toState timestamp

parseTimestamp :: Parser Block
parseTimestamp = do
  char '<'
  content <- takeWhile1P Nothing (/= '>')
  char '>'
  optional newline
  return $ Timestamp (pack ("<" ++ T.unpack content ++ ">"))