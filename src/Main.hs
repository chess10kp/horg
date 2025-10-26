module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (pack)
import System.IO (getContents)
import Text.Megaparsec (parse, errorBundlePretty)

import Org.Json
import Org.Parser
import Org.Types

main :: IO ()
main = do
  input <- getContents
  case parse parseDocument "" (pack input) of
    Left err -> Prelude.putStrLn $ errorBundlePretty err
    Right doc -> BL.putStrLn $ encode doc
