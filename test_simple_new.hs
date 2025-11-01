import Test.Hspec
import Text.Megaparsec (parse)
import Org.Parser (parseDocument)
import Org.Types
import Data.Text (pack)

main :: IO ()
main = hspec $ do
  describe "Simple Test" $ do
    it "parses simple TODO" $ do
      let input = "* TODO Task\n"
      case parse parseDocument "" (pack input) of
        Right (Document [Headline 1 (Just Todo) title Nothing _]) -> do
          title `shouldBe` pack "Task"
        _ -> fail "Failed to parse simple TODO"