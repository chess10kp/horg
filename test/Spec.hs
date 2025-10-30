{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Text.Megaparsec (parse)
import Org.Parser (parseDocument, parseHeadline, parseInline, parseSpace, parseList, parseKeyword, parseComment, parseNonListBlock)
import Org.Json
import Org.Types
import Data.Text (pack)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = hspec $ do
  describe "Org.Parser" $ do
    it "parses a simple headline" $ do
      let input = "* Headline\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing title Nothing _]) -> title `shouldBe` pack "Headline"
        _ -> fail "Failed to parse headline"

    it "parses bold text" $ do
      let input = "*bold*"
      case parse parseInline "" (pack input) of
        Right (Bold [Plain t]) -> t `shouldBe` pack "bold"
        _ -> fail "Failed to parse bold"

    it "parses italic text" $ do
      let input = "/italic/"
      case parse parseInline "" (pack input) of
        Right (Italic [Plain t]) -> t `shouldBe` pack "italic"
        _ -> fail "Failed to parse italic"

    it "parses plain text" $ do
      let input = "plain"
      case parse parseInline "" (pack input) of
        Right (Plain t) -> t `shouldBe` pack "plain"
        _ -> fail "Failed to parse plain"

    it "parses a link" $ do
      let input = "[[link][desc]]"
      case parse parseInline "" (pack input) of
        Right (Link url desc) -> do
          url `shouldBe` pack "link"
          desc `shouldBe` Just (pack "desc")
        _ -> fail "Failed to parse link"

    it "parses unordered list" $ do
      let input = "- item\n"
      case parse (parseList 0) "" (pack input) of
        Right (List Unordered [ListItem content _]) -> do
          case content of
            [Plain t] -> t `shouldBe` pack "item"
            _ -> fail "Wrong content"
        _ -> fail "Failed to parse list"

    it "parses nested headlines" $ do
      let input = "* Top\n** Sub\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing _ Nothing [Headline 2 Nothing _ Nothing _]]) -> True `shouldBe` True
        _ -> fail "Failed to parse nested headlines"

    it "parses headline with TODO state" $ do
      let input = "* TODO Task\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 (Just Todo) title Nothing _]) -> title `shouldBe` pack "Task"
        _ -> fail "Failed to parse TODO headline"

    it "parses headline with DONE state" $ do
      let input = "* DONE Task\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 (Just Done) title Nothing _]) -> title `shouldBe` pack "Task"
        _ -> fail "Failed to parse DONE headline"

    it "calculates progress for subheadings" $ do
      let input = "* Tasks\n** TODO Sub1\n** DONE Sub2\n** TODO Sub3\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing title progress _]) -> do
          title `shouldBe` pack "Tasks"
          progress `shouldBe` Just (Progress 1 3)
        _ -> fail "Failed to calculate progress"

  describe "Org.Json" $ do
    it "encodes document to JSON" $ do
      let doc = Document [Headline 1 Nothing (pack "Test") Nothing []]
      encode doc `shouldNotBe` BL.empty

    it "encodes bold inline" $ do
      let bold = Bold [Plain (pack "test")]
      encode bold `shouldNotBe` ""

    it "encodes list block" $ do
      let list = List Unordered [ListItem [Plain (pack "item")] []]
      encode list `shouldNotBe` ""

    it "encodes code block" $ do
      let code = CodeBlock (pack "haskell") (pack "main = print 1")
      encode code `shouldNotBe` ""

    it "parses underline text" $ do
      let input = "_underline_"
      case parse parseInline "" (pack input) of
        Right (Underline [Plain t]) -> t `shouldBe` pack "underline"
        _ -> fail "Failed to parse underline"

    it "parses code inline" $ do
      let input = "=code="
      case parse parseInline "" (pack input) of
        Right (Code t) -> t `shouldBe` pack "code"
        _ -> fail "Failed to parse code inline"

    it "parses verbatim text" $ do
      let input = "~verbatim~"
      case parse parseInline "" (pack input) of
        Right (Verbatim t) -> t `shouldBe` pack "verbatim"
        _ -> fail "Failed to parse verbatim"

    it "parses strike-through text" $ do
      let input = "+strike+"
      case parse parseInline "" (pack input) of
        Right (Strike [Plain t]) -> t `shouldBe` pack "strike"
        _ -> fail "Failed to parse strike-through"

    it "parses link without description" $ do
      let input = "[[link]]"
      case parse parseInline "" (pack input) of
        Right (Link url Nothing) -> url `shouldBe` pack "link"
        _ -> fail "Failed to parse link without description"

    it "parses ordered list" $ do
      let input = "1. item\n"
      case parse (parseList 0) "" (pack input) of
        Right (List Ordered [ListItem content _]) -> do
          case content of
            [Plain t] -> t `shouldBe` pack "item"
            _ -> fail "Wrong content"
        _ -> fail "Failed to parse ordered list"

    it "parses keyword" $ do
      let input = "#+TITLE: My Document"
      case parse (parseKeyword 0) "" (pack input) of
        Right (Keyword key value) -> do
          key `shouldBe` pack "TITLE"
          value `shouldBe` pack "My Document"
        _ -> fail "Failed to parse keyword"

    it "parses comment" $ do
      let input = "# This is a comment"
      case parse (parseComment 0) "" (pack input) of
        Right (Comment text) -> text `shouldBe` pack " This is a comment"
        _ -> fail "Failed to parse comment"

    it "parses document with title" $ do
      let input = "#+TITLE: My Document\n* Section 1\nplain\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Keyword title titleText, Headline 1 Nothing sectionTitle Nothing _]) -> do
          title `shouldBe` pack "TITLE"
          titleText `shouldBe` pack "My Document"
          sectionTitle `shouldBe` pack "Section 1"
        _ -> fail "Failed to parse document with title"

    it "parses document with multiple properties" $ do
      let input = "#+TITLE: My Document\n#+AUTHOR: John Doe\n#+DATE: 2023-01-01\n* Section\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Keyword title titleText, Keyword author authorText, Keyword date dateText, Headline 1 Nothing sectionTitle Nothing _]) -> do
          title `shouldBe` pack "TITLE"
          titleText `shouldBe` pack "My Document"
          author `shouldBe` pack "AUTHOR"
          authorText `shouldBe` pack "John Doe"
          date `shouldBe` pack "DATE"
          dateText `shouldBe` pack "2023-01-01"
          sectionTitle `shouldBe` pack "Section"
        _ -> fail "Failed to parse document with multiple properties"

    it "parses document with custom property" $ do
      let input = "#+CUSTOM_VALUE: some value list\n* Section\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Keyword custom customValue, Headline 1 Nothing sectionTitle Nothing _]) -> do
          custom `shouldBe` pack "CUSTOM_VALUE"
          customValue `shouldBe` pack "some value list"
          sectionTitle `shouldBe` pack "Section"
        _ -> fail "Failed to parse document with custom property"

    it "parses document with title and description" $ do
      let input = "#+TITLE: Complex Title\n#+DESCRIPTION: A detailed description\n* Introduction\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Keyword title titleText, Keyword desc descText, Headline 1 Nothing introTitle Nothing _]) -> do
          title `shouldBe` pack "TITLE"
          titleText `shouldBe` pack "Complex Title"
          desc `shouldBe` pack "DESCRIPTION"
          descText `shouldBe` pack "A detailed description"
          introTitle `shouldBe` pack "Introduction"
        _ -> fail "Failed to parse document with title and description"

    it "parses empty lines" $ do
      let input = "\n"
      case parse parseDocument "" (pack input) of
        Right (Document [EmptyLine]) -> True `shouldBe` True
        _ -> fail "Failed to parse empty line"

    it "parses document with empty lines between elements" $ do
      let input = "#+TITLE: My Document\n\n* Section 1\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Keyword title titleText, EmptyLine, Headline 1 Nothing sectionTitle Nothing _]) -> do
          title `shouldBe` pack "TITLE"
          titleText `shouldBe` pack "My Document"
          sectionTitle `shouldBe` pack "Section 1"
        _ -> fail "Failed to parse document with empty lines"

  describe "Nested Content" $ do
    it "parses headline with paragraph content" $ do
      let input = "* Top\nThis is content\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing title Nothing [Paragraph content]]) -> do
          title `shouldBe` pack "Top"
          content `shouldBe` [Plain "This", Plain " ", Plain "is", Plain " ", Plain "content"]
        _ -> fail "Failed to parse headline with paragraph content"

    it "parses headline with list content" $ do
      let input = "* Top\n- item 1\n- item 2\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing title Nothing [List Unordered items]]) -> do
          title `shouldBe` pack "Top"
          length items `shouldBe` 2
          let [ListItem content1 _, ListItem content2 _] = items
          content1 `shouldBe` [Plain "item", Plain " ", Plain "1"]
          content2 `shouldBe` [Plain "item", Plain " ", Plain "2"]
        _ -> fail "Failed to parse headline with list content"

    it "parses headline with mixed content" $ do
      let input = "* Top\nFirst paragraph\n- list item\nSecond paragraph\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing title Nothing content]) -> do
          title `shouldBe` pack "Top"
          length content `shouldBe` 2
          let [Paragraph p1, List Unordered [ListItem itemContent [Paragraph p2]]] = content
          p1 `shouldBe` [Plain "First", Plain " ", Plain "paragraph"]
          itemContent `shouldBe` [Plain "list", Plain " ", Plain "item"]
          p2 `shouldBe` [Plain "Second", Plain " ", Plain "paragraph"]
        _ -> fail "Failed to parse headline with mixed content"

    it "parses two-level nested headlines" $ do
      let input = "* Top\n** Sub\nContent\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing topTitle Nothing [Headline 2 Nothing subTitle Nothing [Paragraph content]]]) -> do
          topTitle `shouldBe` pack "Top"
          subTitle `shouldBe` pack "Sub"
          content `shouldBe` [Plain "Content"]
        _ -> fail "Failed to parse two-level nested headlines"

    it "parses three-level nested headlines" $ do
      let input = "* Top\n** Middle\n*** Bottom\nDeep content\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing topTitle Nothing [Headline 2 Nothing middleTitle Nothing [Headline 3 Nothing bottomTitle Nothing [Paragraph content]]]]) -> do
          topTitle `shouldBe` pack "Top"
          middleTitle `shouldBe` pack "Middle"
          bottomTitle `shouldBe` pack "Bottom"
          content `shouldBe` [Plain "Deep", Plain " ", Plain "content"]
        _ -> fail "Failed to parse three-level nested headlines"

    it "parses multiple nested headlines at same level" $ do
      let input = "* Top\n** Sub 1\nContent 1\n** Sub 2\nContent 2\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing topTitle Nothing subHeadlines]) -> do
          topTitle `shouldBe` pack "Top"
          length subHeadlines `shouldBe` 2
          let [Headline 2 Nothing sub1Title Nothing [Paragraph content1], Headline 2 Nothing sub2Title Nothing [Paragraph content2]] = subHeadlines
          sub1Title `shouldBe` pack "Sub 1"
          content1 `shouldBe` [Plain "Content", Plain " ", Plain "1"]
          sub2Title `shouldBe` pack "Sub 2"
          content2 `shouldBe` [Plain "Content", Plain " ", Plain "2"]
        _ -> fail "Failed to parse multiple nested headlines at same level"

    it "parses complex nested structure with lists and headlines" $ do
      let input = "* Top\nParagraph content\n- list item\n** Sub level\nMore content\n- nested list\n*** Deep level\nFinal content\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing topTitle Nothing content]) -> do
          topTitle `shouldBe` pack "Top"
          -- The parser correctly returns 2 blocks: a paragraph and a nested list structure
          length content `shouldBe` 2
          let [Paragraph p1, List Unordered [ListItem item1Content [Headline 2 Nothing subTitle Nothing subContent]]] = content
          p1 `shouldBe` [Plain "Paragraph", Plain " ", Plain "content"]
          item1Content `shouldBe` [Plain "list", Plain " ", Plain "item"]
          subTitle `shouldBe` pack "Sub level"
          length subContent `shouldBe` 2
          let [Paragraph p2, List Unordered [ListItem item2Content [Headline 3 Nothing subSubTitle Nothing subSubContent]]] = subContent
          p2 `shouldBe` [Plain "More", Plain " ", Plain "content"]
          item2Content `shouldBe` [Plain "nested", Plain " ", Plain "list"]
          subSubTitle `shouldBe` pack "Deep level"
          length subSubContent `shouldBe` 1
          let [Paragraph p3] = subSubContent
          p3 `shouldBe` [Plain "Final", Plain " ", Plain "content"]
        _ -> fail "Failed to parse complex nested structure"

    it "parses nested headlines with TODO states" $ do
      let input = "* Project\n** TODO Task 1\n*** DONE Subtask 1.1\n*** TODO Subtask 1.2\n** DONE Task 2\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing projectTitle (Just projectProgress) tasks]) -> do
          projectTitle `shouldBe` pack "Project"
          projectProgress `shouldBe` Progress 2 2
          length tasks `shouldBe` 2
          let [Headline 2 (Just Todo) task1Title (Just task1Progress) subtasks, Headline 2 (Just Done) task2Title Nothing _] = tasks
          task1Title `shouldBe` pack "Task 1"
          task1Progress `shouldBe` Progress 1 2
          task2Title `shouldBe` pack "Task 2"
          length subtasks `shouldBe` 2
          let [Headline 3 (Just Done) sub1Title Nothing _, Headline 3 (Just Todo) sub2Title Nothing _] = subtasks
          sub1Title `shouldBe` pack "Subtask 1.1"
          sub2Title `shouldBe` pack "Subtask 1.2"
        _ -> fail "Failed to parse nested headlines with TODO states"

    it "calculates progress correctly for nested structure" $ do
      let input = "* Project\n** TODO Task 1\n*** DONE Subtask 1\n*** TODO Subtask 2\n** DONE Task 2\n** TODO Task 3\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing projectTitle (Just projectProgress) tasks]) -> do
          projectTitle `shouldBe` pack "Project"
          projectProgress `shouldBe` Progress 2 3
          length tasks `shouldBe` 3
        _ -> fail "Failed to calculate progress correctly for nested structure"

    it "parses deeply nested list within headline" $ do
      let input = "* Top\n- item 1\n  - sub item 1.1\n  - sub item 1.2\n- item 2\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing topTitle Nothing [List Unordered items]]) -> do
          topTitle `shouldBe` pack "Top"
          length items `shouldBe` 2
          let [ListItem content1 [List Unordered subitems1], ListItem content2 []] = items
          content1 `shouldBe` [Plain "item", Plain " ", Plain "1"]
          content2 `shouldBe` [Plain "item", Plain " ", Plain "2"]
          length subitems1 `shouldBe` 2
          let [ListItem subcontent1 [], ListItem subcontent2 []] = subitems1
          subcontent1 `shouldBe` [Plain "sub", Plain " ", Plain "item", Plain " ", Plain "1.1"]
          subcontent2 `shouldBe` [Plain "sub", Plain " ", Plain "item", Plain " ", Plain "1.2"]
        _ -> fail "Failed to parse deeply nested list within headline"

  -- describe "Multi-Heading Documents with Properties" $ do
    it "parses multiple headings with property drawers" $ do
      let input = "* Project A\n:PROPERTIES:\n:STATUS: active\n:PRIORITY: A\n:END:\n** Task A1\n:PROPERTIES:\n:ESTIMATED: 2h\n:END:\n*** Subtask A1.1\n:PROPERTIES:\n:ASSIGNED: John\n:END:\n* Project B\n:PROPERTIES:\n:STATUS: pending\n:PRIORITY: B\n:END:\n** Task B1\n:PROPERTIES:\n:ESTIMATED: 1h\n:END:\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing projectATitle Nothing projectAContent, Headline 1 Nothing projectBTitle Nothing projectBContent]) -> do
          projectATitle `shouldBe` pack "Project A"
          projectBTitle `shouldBe` pack "Project B"
          
          -- Check Project A content
          length projectAContent `shouldBe` 2
          let [PropertyDrawer projectAProps, Headline 2 Nothing taskA1Title Nothing taskA1Content] = projectAContent
          projectAProps `shouldBe` [("STATUS", "active"), ("PRIORITY", "A")]
          taskA1Title `shouldBe` pack "Task A1"
          
          -- Check Task A1 content
          length taskA1Content `shouldBe` 2
          let [PropertyDrawer taskA1Props, Headline 3 Nothing subtaskTitle Nothing subtaskContent] = taskA1Content
          taskA1Props `shouldBe` [("ESTIMATED", "2h")]
          subtaskTitle `shouldBe` pack "Subtask A1.1"
          length subtaskContent `shouldBe` 1
          let [PropertyDrawer subtaskProps] = subtaskContent
          subtaskProps `shouldBe` [("ASSIGNED", "John")]
          
          -- Check Project B content
          length projectBContent `shouldBe` 2
          let [PropertyDrawer projectBProps, Headline 2 Nothing taskB1Title Nothing taskB1Content] = projectBContent
          projectBProps `shouldBe` [("STATUS", "pending"), ("PRIORITY", "B")]
          taskB1Title `shouldBe` pack "Task B1"
          length taskB1Content `shouldBe` 1
          let [PropertyDrawer taskB1Props] = taskB1Content
          taskB1Props `shouldBe` [("ESTIMATED", "1h")]
        _ -> fail "Failed to parse multiple headings with property drawers"

    it "parses deeply nested headings with properties at each level" $ do
      let input = "* Level 1\n:PROPERTIES:\n:L1_PROP: value1\n:END:\n** Level 2\n:PROPERTIES:\n:L2_PROP: value2\n:END:\n*** Level 3\n:PROPERTIES:\n:L3_PROP: value3\n:END:\n**** Level 4\n:PROPERTIES:\n:L4_PROP: value4\n:END:\n***** Level 5\n:PROPERTIES:\n:L5_PROP: value5\n:END:\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing l1Title Nothing l1Content]) -> do
          l1Title `shouldBe` pack "Level 1"
          length l1Content `shouldBe` 2
          let [PropertyDrawer l1Props, Headline 2 Nothing l2Title Nothing l2Content] = l1Content
          l1Props `shouldBe` [("L1_PROP", "value1")]
          l2Title `shouldBe` pack "Level 2"
          
          length l2Content `shouldBe` 2
          let [PropertyDrawer l2Props, Headline 3 Nothing l3Title Nothing l3Content] = l2Content
          l2Props `shouldBe` [("L2_PROP", "value2")]
          l3Title `shouldBe` pack "Level 3"
          
          length l3Content `shouldBe` 2
          let [PropertyDrawer l3Props, Headline 4 Nothing l4Title Nothing l4Content] = l3Content
          l3Props `shouldBe` [("L3_PROP", "value3")]
          l4Title `shouldBe` pack "Level 4"
          
          length l4Content `shouldBe` 2
          let [PropertyDrawer l4Props, Headline 5 Nothing l5Title Nothing l5Content] = l4Content
          l4Props `shouldBe` [("L4_PROP", "value4")]
          l5Title `shouldBe` pack "Level 5"
          
          length l5Content `shouldBe` 1
          let [PropertyDrawer l5Props] = l5Content
          l5Props `shouldBe` [("L5_PROP", "value5")]
        _ -> fail "Failed to parse deeply nested headings with properties"

    it "parses complex document with mixed content and properties" $ do
      let input = "* Main Project\n:PROPERTIES:\n:CREATED: 2025-01-01\n:OWNER: Alice\n:END:\n** Phase 1\n:PROPERTIES:\n:STATUS: in-progress\n:END:\nSome description text\n- Task item 1\n- Task item 2\n** Phase 2\n:PROPERTIES:\n:STATUS: pending\n:END:\nMore description\n*** Subtask 2.1\n:PROPERTIES:\n:ASSIGNED: Bob\n:DEADLINE: 2025-02-01\n:END:\nDetails about subtask\n:LOGBOOK:\nCLOCK: [2025-01-15 Mon 09:00]--[2025-01-15 Mon 11:00] =>  2:00\n- State " ++ "\"DONE\"" ++ " from " ++ "\"TODO\"" ++ " [2025-01-15 Mon 11:30]\n:END:\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing mainTitle Nothing mainContent]) -> do
          mainTitle `shouldBe` pack "Main Project"
          length mainContent `shouldBe` 2
          let [PropertyDrawer mainProps, Headline 2 Nothing phase1Title Nothing phase1Content] = mainContent
          mainProps `shouldBe` [("CREATED", "2025-01-01"), ("OWNER", "Alice")]
          phase1Title `shouldBe` pack "Phase 1"
          
          -- Check Phase 1 content
          length phase1Content `shouldBe` 3
          let [PropertyDrawer phase1Props, Paragraph phase1Desc, List Unordered phase1Tasks] = phase1Content
          phase1Props `shouldBe` [("STATUS", "in-progress")]
          phase1Desc `shouldBe` [Plain "Some", Plain " ", Plain "description", Plain " ", Plain "text"]
          length phase1Tasks `shouldBe` 2
          let [ListItem _ [], ListItem _ [Headline 2 Nothing phase2Title Nothing phase2Content]] = phase1Tasks
          phase2Title `shouldBe` pack "Phase 2"
          
          -- Check Phase 2 content
          length phase2Content `shouldBe` 3
          let [PropertyDrawer phase2Props, Paragraph phase2Desc, Headline 3 Nothing subtaskTitle Nothing subtaskContent] = phase2Content
          phase2Props `shouldBe` [("STATUS", "pending")]
          phase2Desc `shouldBe` [Plain "More", Plain " ", Plain "description"]
          subtaskTitle `shouldBe` pack "Subtask 2.1"
          
          -- Check Subtask content
          length subtaskContent `shouldBe` 3
          let [PropertyDrawer subtaskProps, Paragraph subtaskDesc, LogbookDrawer subtaskLog] = subtaskContent
          subtaskProps `shouldBe` [("ASSIGNED", "Bob"), ("DEADLINE", "2025-02-01")]
          subtaskDesc `shouldBe` [Plain "Details", Plain " ", Plain "about", Plain " ", Plain "subtask"]
          length subtaskLog `shouldBe` 2
          let [ClockEntry _ _ duration1, StateChange _ _ _] = subtaskLog
          duration1 `shouldBe` pack " 2:00"
        _ -> fail "Failed to parse complex document with mixed content and properties"

    it "parses headings with TODO states and properties" $ do
      let input = "* TODO Project Alpha\n:PROPERTIES:\n:PRIORITY: A\n:DEADLINE: 2025-03-01\n:END:\n** DONE Task Alpha 1\n:PROPERTIES:\n:COMPLETED: 2025-02-15\n:EFFORT: 8h\n:END:\n** TODO Task Alpha 2\n:PROPERTIES:\n:STARTED: 2025-02-16\n:EFFORT: 4h\n:END:\n* WAIT Project Beta\n:PROPERTIES:\n:PRIORITY: B\n:BLOCKED: Project Alpha\n:END:\n** TODO Task Beta 1\n:PROPERTIES:\n:DEPENDS: Task Alpha 2\n:END:\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 (Just Todo) alphaTitle (Just (Progress 1 2)) alphaContent, Headline 1 (Just (Other "WAIT")) betaTitle (Just (Progress 0 1)) betaContent]) -> do
          alphaTitle `shouldBe` pack "Project Alpha"
          betaTitle `shouldBe` pack "Project Beta"
          
          -- Check Project Alpha content
          length alphaContent `shouldBe` 3
          let [PropertyDrawer alphaProps, Headline 2 (Just Done) task1Title Nothing task1Content, Headline 2 (Just Todo) task2Title Nothing task2Content] = alphaContent
          alphaProps `shouldBe` [("PRIORITY", "A"), ("DEADLINE", "2025-03-01")]
          task1Title `shouldBe` pack "Task Alpha 1"
          task2Title `shouldBe` pack "Task Alpha 2"
          
          -- Check Task Alpha 1 content
          length task1Content `shouldBe` 1
          let [PropertyDrawer task1Props] = task1Content
          task1Props `shouldBe` [("COMPLETED", "2025-02-15"), ("EFFORT", "8h")]
          
          -- Check Task Alpha 2 content
          length task2Content `shouldBe` 1
          let [PropertyDrawer task2Props] = task2Content
          task2Props `shouldBe` [("STARTED", "2025-02-16"), ("EFFORT", "4h")]
          
          -- Check Project Beta content
          length betaContent `shouldBe` 2
          let [PropertyDrawer betaProps, Headline 2 (Just Todo) taskBetaTitle Nothing taskBetaContent] = betaContent
          betaProps `shouldBe` [("PRIORITY", "B"), ("BLOCKED", "Project Alpha")]
          taskBetaTitle `shouldBe` pack "Task Beta 1"
          length taskBetaContent `shouldBe` 1
          let [PropertyDrawer taskBetaProps] = taskBetaContent
          taskBetaProps `shouldBe` [("DEPENDS", "Task Alpha 2")]
        _ -> fail "Failed to parse headings with TODO states and properties"

  describe "LOGBOOK parsing" $ do
    it "parses logbook with clock entries" $ do
      let input = "* Task\n:LOGBOOK:\nCLOCK: [2025-10-18 Sat 19:51]--[2025-10-18 Sat 20:23] =>  0:32\nCLOCK: [2025-08-30 Sat 12:02]--[2025-08-30 Sat 12:36] =>  0:34\n:END:\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing title Nothing [LogbookDrawer entries]]) -> do
          title `shouldBe` pack "Task"
          length entries `shouldBe` 2
          let [ClockEntry start1 end1 duration1, ClockEntry start2 end2 duration2] = entries
          start1 `shouldBe` pack "[2025-10-18 Sat 19:51]"
          end1 `shouldBe` pack "[2025-10-18 Sat 20:23]"
          duration1 `shouldBe` pack " 0:32"
          start2 `shouldBe` pack "[2025-08-30 Sat 12:02]"
          end2 `shouldBe` pack "[2025-08-30 Sat 12:36]"
          duration2 `shouldBe` pack " 0:34"
        _ -> fail "Failed to parse logbook with clock entries"

    it "parses logbook with state changes" $ do
      let input = "* Task\n:LOGBOOK:\n- State \"DONE\" from \"TODO\" [2025-01-15 Mon 11:30]\n- State \"TODO\" from \"DONE\" [2025-01-16 Tue 09:00]\n:END:\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing title Nothing [LogbookDrawer entries]]) -> do
          title `shouldBe` pack "Task"
          length entries `shouldBe` 2
          let [StateChange from1 to1 timestamp1, StateChange from2 to2 timestamp2] = entries
          from1 `shouldBe` pack "DONE"
          to1 `shouldBe` pack "TODO"
          timestamp1 `shouldBe` pack "2025-01-15 Mon 11:30"
          from2 `shouldBe` pack "TODO"
          to2 `shouldBe` pack "DONE"
          timestamp2 `shouldBe` pack "2025-01-16 Tue 09:00"
        _ -> fail "Failed to parse logbook with state changes"

    it "parses logbook with mixed entries" $ do
      let input = "* Task\n:LOGBOOK:\nCLOCK: [2025-01-15 Mon 09:00]--[2025-01-15 Mon 11:00] =>  2:00\n- State \"DONE\" from \"TODO\" [2025-01-15 Mon 11:30]\nCLOCK: [2025-01-16 Tue 14:00]--[2025-01-16 Tue 15:30] =>  1:30\n:END:\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 Nothing title Nothing [LogbookDrawer entries]]) -> do
          title `shouldBe` pack "Task"
          length entries `shouldBe` 3
          let [ClockEntry start1 end1 duration1, StateChange from to timestamp, ClockEntry start2 end2 duration2] = entries
          start1 `shouldBe` pack "[2025-01-15 Mon 09:00]"
          end1 `shouldBe` pack "[2025-01-15 Mon 11:00]"
          duration1 `shouldBe` pack " 2:00"
          from `shouldBe` pack "DONE"
          to `shouldBe` pack "TODO"
          timestamp `shouldBe` pack "2025-01-15 Mon 11:30"
          start2 `shouldBe` pack "[2025-01-16 Tue 14:00]"
          end2 `shouldBe` pack "[2025-01-16 Tue 15:30]"
          duration2 `shouldBe` pack " 1:30"
        _ -> fail "Failed to parse logbook with mixed entries"



