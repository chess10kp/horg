import Text.Megaparsec (parse)
import Org.Parser (parseDocument)
import Org.Types
import Data.Text (pack)

-- Test case mimicking real.org structure
testInput :: String
testInput = "#+title: Todos\n#+STARTUP: content align\n\n* Todos [0/18]\n** WORK [#A] jobs :jobsearch:daily:\n:PROPERTIES:\n:LAST_REPEAT: [2025-09-02 Tue 16:18]\n:END:\n<2025-09-05 Fri ++1d>\n:LOGBOOK:\nCLOCK: [2025-10-18 Sat 19:51]--[2025-10-18 Sat 20:23] =>  0:32\n:END:\n*** [[file:~/projects/repos/New-Grad-2026/][new grad]]\n*** [[file:~/projects/repos/New-Grad-Positions/][more new grad]]\n** TODO [#A] daily note :personal:daily:\n:LOGBOOK:\nCLOCK: [2025-10-09 Thu 22:56]--[2025-10-09 Thu 23:16] =>  0:20\n:END:\n** TODO Call Mike and ask about full time\nDEADLINE: <2025-12-09 Tue>\n"

main :: IO ()
main = do
  putStrLn "Testing real.org structure..."
  putStrLn $ "Input length: " ++ show (length testInput)
  case parse parseDocument "" (pack testInput) of
    Right (Document blocks) -> do
      putStrLn $ "Parsed " ++ show (length blocks) ++ " top-level blocks"
      mapM_ printBlock blocks
    Left err -> putStrLn $ "Error: " ++ show err

printBlock :: Block -> IO ()
printBlock (Headline level todo title progress content) = do
  putStrLn $ "Headline level " ++ show level ++ ": " ++ show title
  putStrLn $ "  Content blocks: " ++ show (length content)
  mapM_ (\b -> putStrLn $ "    " ++ getBlockType b) content
printBlock (Keyword key value) = putStrLn $ "Keyword: " ++ show key ++ " = " ++ show value
printBlock EmptyLine = putStrLn "EmptyLine"
printBlock block = putStrLn $ "Other: " ++ getBlockType block

getBlockType :: Block -> String
getBlockType (Headline _ _ _ _ _) = "Headline"
getBlockType (Paragraph _) = "Paragraph"
getBlockType (List _ _) = "List"
getBlockType (CodeBlock _ _) = "CodeBlock"
getBlockType (Keyword _ _) = "Keyword"
getBlockType (PropertyDrawer _) = "PropertyDrawer"
getBlockType (LogbookDrawer _) = "LogbookDrawer"
getBlockType (Timestamp _) = "Timestamp"
getBlockType (Table _) = "Table"
getBlockType (Comment _) = "Comment"
getBlockType EmptyLine = "EmptyLine"