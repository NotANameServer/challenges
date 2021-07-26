module Connect4.GameSpec (main, spec) where

import Control.Monad.ST
import Data.Massiv.Array 
import Data.Vector.Unboxed as U
import Test.Hspec

import Connect4.Game

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

    describe "init" $ do

        it "mkGame 1" $ do
            g <- stToIO $ mkGame PlayerR
            _firstPlayer g `shouldBe` PlayerR
            _status g `shouldBe` PlayR
            _currentPlayer g `shouldBe` PlayerR
            _moves g `shouldBe` U.fromList [0 .. 6]

        it "nextGame 1" $ do
            g <- stToIO (mkGame PlayerR >>= nextGame)
            _status g `shouldBe` PlayY
            _firstPlayer g `shouldBe` PlayerY
            _currentPlayer g `shouldBe` PlayerY
            _moves g `shouldBe` U.fromList [0 .. 6]

    describe "lineLength" $ do

        it "lineLength 1" $ do
            l1 <- stToIO (newMArray (Sz2 nI nJ) CellE
                            >>= lineLength 0 1 0 1 CellR)
            l1 `shouldBe` 0

        it "lineLength 2" $ do
            l1 <- stToIO $ do 
                    b0 <- newMArray (Sz2 nI nJ) CellE
                    writeM b0 (Ix2 0 2) CellR
                    writeM b0 (Ix2 0 3) CellR
                    lineLength 0 1 0 1 CellR b0
            l1 `shouldBe` 2

        it "lineLength 3" $ do
            l1 <- stToIO $ do 
                    b0 <- newMArray (Sz2 nI nJ) CellE
                    writeM b0 (Ix2 0 0) CellR
                    writeM b0 (Ix2 0 1) CellR
                    lineLength 0 2 0 (-1) CellR b0
            l1 `shouldBe` 2

    describe "playK" $ do

        it "playK 1" $ do
            g <- stToIO (mkGame PlayerR >>= playK 1)
            _status g `shouldBe` PlayY
            _firstPlayer g `shouldBe` PlayerR
            _currentPlayer g `shouldBe` PlayerY
            _moves g `shouldBe` U.fromList [0 .. 6]

        it "playK 2" $ do
            g <- stToIO (mkGame PlayerR 
                        >>= playK 6
                        >>= playK 6
                        >>= playK 6
                        >>= playK 6
                        >>= playK 6
                        >>= playK 6)
            _status g `shouldBe` PlayR
            _firstPlayer g `shouldBe` PlayerR
            _currentPlayer g `shouldBe` PlayerR
            _moves g `shouldBe` U.fromList [0 .. 5]

        it "playK 3" $ do
            g <- stToIO (mkGame PlayerR 
                        >>= playK 2 >>= playK 4
                        >>= playK 2 >>= playK 4
                        >>= playK 2 >>= playK 4
                        >>= playK 2)
            _status g `shouldBe` WinR
            _firstPlayer g `shouldBe` PlayerR
            _currentPlayer g `shouldBe` PlayerY
            _moves g `shouldBe` U.fromList []

