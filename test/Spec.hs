import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
    describe "Test Convert" $ do
        it "header" $ do
            htmlString <- md2html "# h1"
            htmlString `shouldBe` "<h1>h1</h1>"

        it "link" $ do
            md2html' "[base](url)" `shouldBe` "<p><a href=\"url\">base</a></p>"
            md2html' "![name](url)" `shouldBe` "<p><img src=\"url\" alt=\"name\" /></p>"