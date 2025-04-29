import Text.Pandoc
import Test.Hspec
import Test.QuickCheck

import Lib

main :: IO ()
main = hspec $ do
    describe "Test Convert" $ do
        it "header" $ do
            htmlString <- md2html "# h1"
            htmlString `shouldBe` "<h1>h1</h1>"
