import Text.Megaparsec (parse)
import Org.Parser (parseDocument, parseBlock)
import Org.Types
import Data.Text (pack)

-- Simple test case
testInput :: String
testInput = "#+title: Test\n\n* Headline 1\n** Sub 1\nContent 1\n** Sub 2\nContent 2\n* Headline 2\nContent 3\n"

main :: IO ()
main = do
  putStrLn "Testing simple case..."
  putStrLn $ "Input:\n" ++ testInput
  case parse parseDocument "" (pack testInput) of
    Right (Document blocks) -> do
      putStrLn $ "Parsed " ++ show (length blocks) ++ " top-level blocks"
      mapM_ (putStrLn . ("  " ++)) (map show blocks)
    Left err -> putStrLn $ "Error: " ++ show err