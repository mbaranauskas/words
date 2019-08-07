import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
    describe "formatGrid" $ do
        it "Should concatenate every line with a newline" $ do
            (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

    describe "findWord" $ do
        it "Should find words that exist on the Grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL" 
            findWord grid "PERL" `shouldBe` Just "PERL"    
            findWord grid "RUBY" `shouldBe` Just "RUBY"   
            findWord grid "COBOL" `shouldBe` Just "COBOL"     

    describe "findWord" $ do
        it "Should return 'Nothing' for words that does not exist on the Grid" $ do
            findWord grid "Mindaugas" `shouldBe` Nothing        
        
