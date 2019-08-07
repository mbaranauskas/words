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
        it "Should return 'Nothing' for words that does not exist on the Grid" $ do
            findWord grid "Mindaugas" `shouldBe` Nothing                
        
    describe "findWords" $ do
        it "Should find all words that exist on the Grid" $ do 
            findWords grid languages `shouldBe` languages   
        it "Should not find words that do not exist" $ do
            findWords grid ["Mindaugas", "Robertas", "Jonas"] `shouldBe` [] 
