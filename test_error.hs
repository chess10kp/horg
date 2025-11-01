import Text.Megaparsec (parse)
import Org.Parser (parseDocument)
import Data.Text (pack)

main :: IO ()
main = do
  let input = pack "* Task with invalid logbook entry\n:LOGBOOK:\nCLOCK: [2025-01-01 Wed 09:00]--[2025-01-01 Wed 10:00] =>  1:00\nSOME_UNKNOWN_ENTRY: this should fail\n:END:\n"
  case parse parseDocument "" input of
    Left err -> putStrLn $ "Good: Parsing failed with error: " ++ show err
    Right result -> putStrLn $ "Bad: Parsing succeeded when it should have failed: " ++ show result