module Connect4.BotNegamaxSpec (main, spec) where

import Connect4.BotNegamax
import Connect4.Game

import Control.Monad
import Control.Monad.ST
import Test.Hspec

main :: IO ()
main = hspec spec

-- shouldNear :: Double -> Double -> Double -> Expectation
-- shouldNear x1 x2 e = abs (x1 - x2) `shouldSatisfy` (<e)

spec :: Spec
spec = do

    describe "reorder7" $ do
        it "1" $ reorder7 [0..6] `shouldBe` [3,2,4,1,5,0,6]
        it "2" $ reorder7 [0..5] `shouldBe` [3,2,4,1,5,0]
        it "3" $ reorder7 [0,2,1,3] `shouldBe` [3,2,1,0]

    {- 
    describe "reorder" $ do
        it "1" $ reorder [0..6::Int] `shouldBe` [3,2,4,1,5,0,6]
        it "2" $ reorder [0..5::Int] `shouldBe` [3,2,4,1,5,0]
    -}

    describe "newLength" $ do
        it "1" $ newLength CellR 2 0 `shouldBe` kL2
        it "2" $ newLength CellR 2 1 `shouldBe` 1+kL2
        it "3" $ newLength CellR 3 1 `shouldBe` 1+kL3
        it "4" $ newLength CellR 4 1 `shouldBe` 1+kL4
        it "5" $ newLength CellR 5 1 `shouldBe` 1+kL4
        it "6" $ newLength CellY 5 1 `shouldBe` 1-kL4
        it "7" $ newLength CellY 3 1 `shouldBe` 1-kL3
        it "8" $ newLength CellY 2 1 `shouldBe` 1-kL2
        it "9" $ newLength CellY 1 1 `shouldBe` 1
        it "10" $ newLength CellR 1 1 `shouldBe` 1


    describe "countLengths" $ do

        it "1" $ do
            score <- stToIO $ do
                    g <- mkGame PlayerR
                    countLengths g 0 (0, 0, 1, 1) 
            score `shouldBe` 0

        it "2" $ do
            -- .......
            -- .......
            -- .......
            -- .......
            -- ..YY...
            -- .RRR...
            score <- stToIO $ do
                    g0 <- mkGame PlayerR
                    g <- foldM (flip playJ) g0 [3, 3, 2, 2, 1]
                    countLengths g 0 (0, 0, 0, 1)
            score `shouldBe` kL3

        it "3" $ do
            -- .......
            -- .......
            -- .......
            -- .......
            -- .......
            -- .RYRY..
            score <- stToIO $ do
                    g0 <- mkGame PlayerR
                    g <- foldM (flip playJ) g0 [1, 2, 3, 4]
                    countLengths g 0 (0, 0, 0, 1)
            score `shouldBe` 0

        it "4" $ do
            -- .......
            -- .......
            -- .......
            -- .......
            -- .......
            -- YYR.R..
            score <- stToIO $ do
                    g0 <- mkGame PlayerR
                    g <- foldM (flip playJ) g0 [2, 0, 4, 1]
                    countLengths g 0 (0, 0, 0, 1)
            score `shouldBe` (-kL2)

        it "5" $ do
            -- .......
            -- .......
            -- .......
            -- .......
            -- .......
            -- YYYRRRR
            score <- stToIO $ do
                    g0 <- mkGame PlayerR
                    g <- foldM (flip playJ) g0 [3, 0, 4, 1, 5, 2, 6]
                    countLengths g 0 (0, 0, 0, 1)
            score `shouldBe` (kL4 - kL3)

        it "6" $ do
            -- .......
            -- .......
            -- .......
            -- .......
            -- .......
            -- YYYRRRR
            score <- stToIO $ do
                    g0 <- mkGame PlayerR
                    g <- foldM (flip playJ) g0 [3, 0, 4, 1, 5, 2, 6]
                    countLengths g 0 (0, 6, 0, -1)
            score `shouldBe` (kL4 - kL3)
            

        it "7" $ do
            -- .......
            -- .......
            -- .......
            -- .R.....
            -- RY.....
            -- YR.....
            score <- stToIO $ do
                    g0 <- mkGame PlayerR
                    g <- foldM (flip playJ) g0 [1, 0, 0, 1, 1]
                    countLengths g 0 (1, 0, 1, 1)
            score `shouldBe` kL2
            

