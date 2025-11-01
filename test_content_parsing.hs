import Text.Megaparsec (parse)
import Org.Parser (parseBlock)
import Org.Types
import Data.Text (pack)

-- Test parsing the content after the first headline
testInput :: String
testInput = "** TODO [#A] daily note :personal:daily:\n:LOGBOOK:\nCLOCK: [2025-10-09 Thu 22:56]--[2025-10-09 Thu 23:16] =>  0:20\n:END:\n** TODO Call Mike and ask about full time\nDEADLINE: <2025-12-09 Tue>\n"

main :: IO ()
main = do
  putStrLn "Testing content parsing..."
  putStrLn $ "Input:\n" ++ testInput
  case parse (parseBlock 1) "" (pack testInput) of
    Right block -> putStrLn $ "Success: " ++ show block
    Left err -> putStrLn $ "Error: " ++ show err