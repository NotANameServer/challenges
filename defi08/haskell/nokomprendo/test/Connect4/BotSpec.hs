module Connect4.BotSpec (main, spec) where

import Connect4.Bot
import Connect4.Game

import Control.Monad
import Control.Monad.ST
import Data.STRef
import System.Random.MWC
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "BotRandom" $ do

        it "random 1" $ do
            gen <- createSystemRandom 
            let n = 100
            xs <- stToIO $ replicateM n $ uniformR (0, 10::Int) gen
            xs  `shouldSatisfy` all (\x -> x>=0 && x<=10)

        it "random 2" $ do
            gen <- createSystemRandom 
            let n = 10000
            xs <- stToIO $ replicateM n $ uniformR (0, 10::Int) gen
            let m = fromIntegral (sum xs) / fromIntegral n
            abs (m - 5.0 :: Double) `shouldSatisfy` (<0.1)

        it "random 3" $ do
            gen <- createSystemRandom 
            let n = 10000
            xs <- stToIO $ replicateM n $ uniformR (0, 10::Int) gen
            let nD = fromIntegral n
                h = [ fromIntegral (length (filter (==i) xs)) / nD | i <- [0 .. 10] ]
            h `shouldSatisfy` 
                all (\hi -> abs (hi - 0.1 :: Double) < 0.05)

        it "genmove 1" $ do
            br <- BotRandom <$> createSystemRandom
            by <- BotRandom <$> createSystemRandom
            let n = 1000
            let go :: Int -> Game RealWorld -> ST RealWorld [Status]
                go 0 _ = return []
                go i g = do
                    gi <- nextGame g 
                    si <- playoutBots br by gi
                    ss <- go (i-1) gi
                    return (si : ss)
            xs <- stToIO (mkGame PlayerY >>= go n)
            let ratio :: Status -> Double
                ratio status = fromIntegral (length $ filter (==status) xs) / fromIntegral n
            abs (ratio WinR - 0.5) `shouldSatisfy` (<0.05)
            abs (ratio WinY - 0.5) `shouldSatisfy` (<0.05)
            abs (ratio Tie - 0.0) `shouldSatisfy` (<0.05)

    describe "BotMcts" $ do

        it "mkRoot 1" $ do
            x <- stToIO $ mkGame PlayerR >>= mkRoot
            stToIO (readSTRef (nodeReward x)) >>= (`shouldBe` 0)
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 0)
            _status (nodeGame x) `shouldBe` PlayR
            nodePlayer x `shouldBe` PlayerR
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 0)

        it "mkRoot 2" $ do
            x <- stToIO $ mkGame PlayerR >>= playK 6 >>= mkRoot
            stToIO (readSTRef (nodeReward x)) >>= (`shouldBe` 0)
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 0)
            _status (nodeGame x) `shouldBe` PlayY
            nodePlayer x `shouldBe` PlayerY
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 0)

        it "mkRoot 3" $ do
            x <- stToIO $ mkGame PlayerR
                            >>= playK 2
                            >>= playK 2
                            >>= playK 2
                            >>= playK 2
                            >>= playK 2
                            >>= playK 2
                            >>= mkRoot
            stToIO (readSTRef (nodeReward x)) >>= (`shouldBe` 0)
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 0)
            _status (nodeGame x) `shouldBe` PlayR
            nodePlayer x `shouldBe` PlayerR
            nodeNmoves x `shouldBe` 6
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 0)

        it "mkLeaf 1" $ do
            x <- stToIO $ mkGame PlayerR >>= mkRoot >>= newSTRef >>= mkLeaf 2 
            stToIO (readSTRef (nodeReward x)) >>= (`shouldBe` 0)
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 0)
            _status (nodeGame x) `shouldBe` PlayY
            nodePlayer x `shouldBe` PlayerR
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 0)


        it "iterate 1" $ do
            x <- stToIO $ do
                    root <- mkGame PlayerR >>= playK 6 >>= mkRoot
                    leaf <- selectAndExpand root
                    gen <- create
                    status <- simulate gen leaf
                    backpropagate status leaf
                    return root
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 1)
            _status (nodeGame x) `shouldBe` PlayY
            nodePlayer x `shouldBe` PlayerY
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 1)

        it "iterate 2" $ do
            x <- stToIO $ do
                    root <- mkGame PlayerR >>= playK 6 >>= mkRoot
                    replicateM_ 2 $ do
                        leaf <- selectAndExpand root
                        gen <- create
                        status <- simulate gen leaf
                        backpropagate status leaf
                    return root
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 2)
            _status (nodeGame x) `shouldBe` PlayY
            nodePlayer x `shouldBe` PlayerY
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 2)

        it "iterate 3" $ do
            x <- stToIO $ do
                    root <- mkGame PlayerR >>= playK 6 >>= mkRoot
                    replicateM_ 7 $ do
                        leaf <- selectAndExpand root
                        gen <- create
                        status <- simulate gen leaf
                        backpropagate status leaf
                    return root
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 7)
            _status (nodeGame x) `shouldBe` PlayY
            nodePlayer x `shouldBe` PlayerY
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 7)

        it "iterate 4" $ do
            x <- stToIO $ do
                    root <- mkGame PlayerR >>= playK 6 >>= mkRoot
                    replicateM_ 8 $ do
                        leaf <- selectAndExpand root
                        gen <- create
                        status <- simulate gen leaf
                        backpropagate status leaf
                    return root
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 8)
            _status (nodeGame x) `shouldBe` PlayY
            nodePlayer x `shouldBe` PlayerY
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 7)

        it "iterate 5" $ do
            x <- stToIO $ do
                    root <- mkGame PlayerR >>= playK 6 >>= mkRoot
                    replicateM_ 10 $ do
                        leaf <- selectAndExpand root
                        gen <- create
                        status <- simulate gen leaf
                        backpropagate status leaf
                    return root
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 10)
            _status (nodeGame x) `shouldBe` PlayY
            nodePlayer x `shouldBe` PlayerY
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 7)

        it "iterate 6" $ do
            x <- stToIO $ do
                    root <- mkGame PlayerR >>= playK 6 >>= mkRoot
                    replicateM_ 100 $ do
                        leaf <- selectAndExpand root
                        gen <- create
                        status <- simulate gen leaf
                        backpropagate status leaf
                    return root
            stToIO (readSTRef (nodeNsims x)) >>= (`shouldBe` 100)
            _status (nodeGame x) `shouldBe` PlayY
            nodePlayer x `shouldBe` PlayerY
            nodeNmoves x `shouldBe` 7
            stToIO (readSTRef (nodeLastI x)) >>= (`shouldBe` 7)

