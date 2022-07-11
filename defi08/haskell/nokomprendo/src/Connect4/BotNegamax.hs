{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Connect4.BotNegamax where

import Connect4.Bot
import Connect4.Game

import qualified Data.Vector.Unboxed as U
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as MA

import Control.Monad
import Control.Monad.ST
import Data.Maybe

----------------------------------------------------------------------
-- EvalFunc
----------------------------------------------------------------------

type EvalFunc s = Game s -> ST s Int

evalFunc1 :: EvalFunc s
evalFunc1 g = 
    let rows = [(i, 0, 0, 1) | i<-[0 .. nI-1]]
        cols = [(0, j, 1, 0) | j<-[0 .. nJ-1]]
        d1s = [(0, j, 1, 1) | j<-[0 .. nJ-4]] ++ [(i, 0, 1, 1) | i<-[1 .. nI-4]]
        d2s = [(nI-1, j, -1, 1) | j<-[0 .. nJ-4]] ++ [(i, 0, -1, 1) | i<-[3 .. nI-1]]
    in foldM (countLengths g) 0 (rows ++ cols ++ d1s ++ d2s)

kL2, kL3, kL4 :: Int
kL2 = 1
kL3 = 100
kL4 = 10000

kCell :: Cell -> Int
kCell CellR = 1
kCell CellY = -1
kCell CellE = 0

newLength :: Cell -> Int -> Int -> Int
newLength curCell nbCells score 
        | nbCells >= 4 = score + kL4 * kCell curCell
        | nbCells == 3 = score + kL3 * kCell curCell
        | nbCells == 2 = score + kL2 * kCell curCell
        | otherwise = score

countLengths :: Game s -> Int -> (Int, Int, Int, Int) -> ST s Int
countLengths g score0 (i0, j0, di, dj) =
    let cs = _cells g
        go i j curCell nbCells score =
            if not (checkIJ i j)
            then return $ newLength curCell nbCells score
            else do
                c <- MA.readM cs (A.Ix2 i j)
                if c == curCell
                then go (i+di) (j+dj) c (nbCells+1) score
                else go (i+di) (j+dj) c 1 (newLength curCell nbCells score)
    in go i0 j0 CellE 0 score0

----------------------------------------------------------------------
-- BotNegamax
-- https://en.wikipedia.org/wiki/Negamax#Negamax_base_algorithm
----------------------------------------------------------------------

data BotNegamax s = BotNegamax (EvalFunc s) Int

instance Bot s (BotNegamax s) where
    genmove (BotNegamax f d) g = 
        fst <$> negamax f g d (if _currentPlayer g == PlayerR then 1 else -1)

negamax :: EvalFunc s -> Game s -> Int -> Int -> ST s (Int, Int)
negamax f g d c
    | d==0 || not (isRunning g) = do
        v <- f g 
        return (0, -c*v)
    | otherwise = do
        let fAcc (k,v) ki ji = do
                gi <- cloneGame g >>= playJ ji
                (_, vi) <- negamax f gi (d-1) (-c)
                return $ if vi>v then (ki, vi) else (k,v)
        (kb, vb) <- U.ifoldM' fAcc (-1, minBound) (_moves g)
        return (kb, -vb)

----------------------------------------------------------------------
-- BotNegamaxAB
-- https://fr.wikipedia.org/wiki/%C3%89lagage_alpha-b%C3%AAta#Pseudocode
----------------------------------------------------------------------

data BotNegamaxAB s = BotNegamaxAB (EvalFunc s) Int

instance Bot s (BotNegamaxAB s) where
    genmove (BotNegamaxAB f d) g = do
        let c = if _currentPlayer g == PlayerR then 1 else -1
        (j, _) <- negamaxAB f g d minBound c
        return $ fromJust $ U.elemIndex j (_moves g)

negamaxAB :: EvalFunc s -> Game s -> Int -> Int -> Int -> ST s (Int, Int)
negamaxAB f g d a c
    | d==0 || not (isRunning g) = do
        v <- f g 
        return (0, -c*v)
    | otherwise = 
        let go j v [] = return (j,-v)
            go j v (ji:js) = do
                gi <- cloneGame g >>= playJ ji
                (_, vi) <- negamaxAB f gi (d-1) v (-c)
                let mi = max v vi
                if (-mi) <= a
                then return (j,-mi)
                else if vi>v then go ji vi js
                             else go j v js
        in go (-1) minBound (reorder7 $ U.toList $ _moves g)

reorder7 :: [Int] -> [Int]
reorder7 xs = filter (`elem` xs) [3,2,4,1,5,0,6]

