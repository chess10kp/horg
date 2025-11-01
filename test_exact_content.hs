import Text.Megaparsec (parse)
import Org.Parser (parseBlock)
import Org.Types
import Data.Text (pack)

-- Exact content from real.org after the first headline and its first sub-headline
testInput :: String
testInput = "*** [[file:~/projects/repos/New-Grad-2026/][new grad]]\n*** [[file:~/projects/repos/New-Grad-Positions/][more new grad]]\n** TODO [#A] daily note :personal:daily:\n:LOGBOOK:\nCLOCK: [2025-10-09 Thu 22:56]--[2025-10-09 Thu 23:16] =>  0:20\n:END:\n** TODO Call Mike and ask about full time\nDEADLINE: <2025-12-09 Tue>\n"

main :: IO ()
main = do
  putStrLn "Testing exact content from real.org..."
  putStrLn $ "Input length: " ++ show (length testInput)
  case parse (parseBlock 2) "" (pack testInput) of
    Right block -> putStrLn $ "Success: " ++ show block
    Left err -> putStrLn $ "Error: " ++ show err