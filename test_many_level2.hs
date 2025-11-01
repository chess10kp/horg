import Text.Megaparsec (parse, many)
import Org.Parser (parseBlock)
import Org.Types
import Data.Text (pack)

-- Test many blocks at level 2
testInput :: String
testInput = "*** [[file:~/projects/repos/New-Grad-2026/][new grad]]\n*** [[file:~/projects/repos/New-Grad-Positions/][more new grad]]\n** TODO [#A] daily note :personal:daily:\n:LOGBOOK:\nCLOCK: [2025-10-09 Thu 22:56]--[2025-10-09 Thu 23:16] =>  0:20\n:END:\n** TODO Call Mike and ask about full time\nDEADLINE: <2025-12-09 Tue>\n"

main :: IO ()
main = do
  putStrLn "Testing many blocks at level 2..."
  case parse (many (parseBlock 2)) "" (pack testInput) of
    Right blocks -> do
      putStrLn $ "Success: parsed " ++ show (length blocks) ++ " blocks"
      mapM_ (putStrLn . ("  " ++)) (map show blocks)
    Left err -> putStrLn $ "Error: " ++ show err