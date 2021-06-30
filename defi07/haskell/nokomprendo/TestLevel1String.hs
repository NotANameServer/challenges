import Level1String hiding (main)

import Test.Hspec

main :: IO ()
main = hspec $ do

    describe "1 2 3 * +" $ do
        it "run" $ run (words " 1 2 3 * +") `shouldBe` Just ([], 7)

    describe "1 2 + 3 - 4 +" $ do
        it "run" $ run (words "1 2 + 3 - 4 +") `shouldBe` Just ([], 4)

    describe "1 2 3 * +" $ do
        it "run" $ run (words "1 2 3 * +") `shouldBe` Just ([], 7)

    describe "3 2 ! ^" $ do
        it "run" $ run (words "3 2 ! ^") `shouldBe` Just ([], 9)

    describe "3 ! 2 ^" $ do
        it "run" $ run (words "3 ! 2 ^") `shouldBe` Just ([], 36)

    describe "2 3 ^ 2 *" $ do
        it "run" $ run (words "2 3 ^ 2 *") `shouldBe` Just ([], 16)

    describe "2 2 3 ^ *" $ do
        it "run" $ run (words "2 2 3 ^ *") `shouldBe` Just ([], 16)

    describe "1 3 ! +" $ do
        it "run" $ run (words "1 3 ! +") `shouldBe` Just ([], 7)

    describe "2 3 2 ^ ^" $ do
        it "run" $ run (words "2 3 2 ^ ^") `shouldBe` Just ([], 512)

    describe "1 2 3 + 4 * +" $ do
        it "run" $ run (words "1 2 3 + 4 * +") `shouldBe` Just ([], 21)

    describe "1 2 3 + 4 * + 2 *" $ do
        it "run" $ run (words "1 2 3 + 4 * + 2 *") `shouldBe` Just ([], 42)

    describe "16 4 / 2 /" $ do
        it "run" $ run (words "16 4 / 2 /") `shouldBe` Just ([], 2)

