{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Text.Megaparsec (parse)
import Org.Parser
import Org.Json
import Org.Types
import Data.Text (pack)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = hspec $ do
  describe "Org.Parser" $ do
    it "parses a simple headline" $ do
      let input = "* Headline\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 title _]) -> title `shouldBe` pack "Headline"
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
      case parse parseList "" (pack input) of
        Right (List Unordered [ListItem content _]) -> do
          case content of
            [Plain t] -> t `shouldBe` pack "item"
            _ -> fail "Wrong content"
        _ -> fail "Failed to parse list"

  describe "Org.Json" $ do
    it "encodes document to JSON" $ do
      let doc = Document [Headline 1 (pack "Test") []]
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