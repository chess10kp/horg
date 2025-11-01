import Text.Megaparsec (parse)
import Org.Parser (parseBlock)
import Org.Types
import Data.Text (pack)
import Text.Megaparsec (many)

-- Test parsing multiple blocks
testInput :: String
testInput = "** TODO [#A] daily note :personal:daily:\n:LOGBOOK:\nCLOCK: [2025-10-09 Thu 22:56]--[2025-10-09 Thu 23:16] =>  0:20\n:END:\n** TODO Call Mike and ask about full time\nDEADLINE: <2025-12-09 Tue>\n"

main :: IO ()
main = do
  putStrLn "Testing many blocks parsing..."
  putStrLn $ "Input:\n" ++ testInput
  case parse (many (parseBlock 1)) "" (pack testInput) of
    Right blocks -> do
      putStrLn $ "Success: parsed " ++ show (length blocks) ++ " blocks"
      mapM_ (putStrLn . ("  " ++)) (map show blocks)
    Left err -> putStrLn $ "Error: " ++ show err