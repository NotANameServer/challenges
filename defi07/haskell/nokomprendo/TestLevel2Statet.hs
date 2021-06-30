-- import Level2Adt hiding (main)
import Level2Statet hiding (main)

import Test.Hspec

main :: IO ()
main = hspec $ do

    describe "1 + 2 * 3" $ do
        it "runParser" $ runParser additiveP "1 + 2 * 3" `shouldBe` Just (7, "")

    describe "1+2-3+4" $ do
        it "runParser" $ runParser additiveP "1+2-3+4" `shouldBe` Just (4, "")

    describe "1+2*3" $ do
        it "runParser" $ runParser additiveP "1+2*3" `shouldBe` Just (7, "")

    describe "3^2!" $ do
        it "runParser" $ runParser additiveP "3^2!" `shouldBe` Just (9, "")

    describe "3!^2" $ do
        it "runParser" $ runParser additiveP "3!^2" `shouldBe` Just (36, "")

    describe "2^3*2" $ do
        it "runParser" $ runParser additiveP "2^3*2" `shouldBe` Just (16, "")

    describe "2*2^3" $ do
        it "runParser" $ runParser additiveP "2*2^3" `shouldBe` Just (16, "")

    describe "1+3!" $ do
        it "runParser" $ runParser additiveP "1+3!" `shouldBe` Just (7, "")

    describe "2^3^2" $ do
        it "runParser" $ runParser additiveP "2^3^2" `shouldBe` Just (512, "")

    describe "1 + (2 + 3) * 4" $ do
        it "runParser" $ runParser additiveP "1 + (2 + 3) * 4" `shouldBe` Just (21, "")

    describe "( 1 + (2 + 3) * 4 )* 2" $ do
        it "runParser" $ runParser additiveP "( 1 + (2 + 3) * 4 )* 2" `shouldBe` Just (42, "")

    describe "16/4/2" $ do
        it "runParser" $ runParser additiveP "16/4/2" `shouldBe` Just (2, "")

    describe "2.3" $ do
        it "runParser" $ runParser additiveP "2.3" `shouldBe` Just (2.3, "")

    describe "-2.3" $ do
        it "runParser" $ runParser additiveP "-2.3" `shouldBe` Just (-2.3, "")

    describe "3--2.3" $ do
        it "runParser" $ runParser additiveP "3--2.3" `shouldBe` Just (5.3, "")

    describe "3 - -2.3" $ do
        it "runParser" $ runParser additiveP "3 - -2.3" `shouldBe` Just (5.3, "")

    describe "3.3!" $ do
        it "runParser" $ runParser additiveP "3.3!" `shouldBe` Just (6, "")

    describe "-3.3!" $ do
        it "runParser" $ runParser additiveP "-3.3!" `shouldBe` Just (1, "")

