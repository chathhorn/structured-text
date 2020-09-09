import Prelude hiding (readFile)
import System.Directory (listDirectory)
import Data.Text (Text)
import Data.List (isSuffixOf)
import Data.Text.IO (readFile)
import System.FilePath ((</>), takeBaseName, takeFileName)
import Test.Tasty
import Test.Tasty.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec (parse)
import StructuredText.Parser (parseTop)
import Paths_structured_text (getDataFileName)

main :: IO ()
main = do
      fps   <- stFiles
      txts  <- mapM readFile fps
      tests <- mapM (testSpecs . uncurry spec) $ zip fps txts
      defaultMain $ testGroup "Tests" (concat tests)

spec :: FilePath -> Text -> Spec
spec fp = describe ("parse_" ++ takeBaseName fp)
      . it ("should parse the file " ++ takeFileName fp)
      . (parse parseTop fp `shouldSucceedOn`)

stFiles :: IO [FilePath]
stFiles = do
      dir <- getDataFileName "samples"
      map (dir </>) . filter (".st" `isSuffixOf`) <$> listDirectory dir
