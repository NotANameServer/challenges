{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

import Connect4.Bot
import Connect4.BotNegamax
import Connect4.Game

import qualified Data.Vector.Unboxed as U
import Data.Massiv.Array hiding (map, reverse)

import Control.Monad
import Control.Monad.ST
import Data.List.Split
import System.Environment
import System.IO
import System.Random.MWC
import Text.Read (readMaybe)

----------------------------------------------------------------------
-- game
----------------------------------------------------------------------

formatCell :: Cell -> String
formatCell CellE = "."
formatCell CellR = "R"
formatCell CellY = "Y"

showGame :: Game s -> ST s String
showGame g = do
    cs <- reverse . toLists2 <$> freezeS (_cells g)
    let bb = unlines $ map (concatMap formatCell) cs
    return $ "\n0123456\n" ++ bb 
        ++ "moves: " ++ unwords (map show $ U.toList $ _moves g)
        ++ "\nstatus: " ++ show (_status g) ++ "\n"

----------------------------------------------------------------------
-- bots
----------------------------------------------------------------------

class BotIO b where
    genmoveIO :: b -> Game RealWorld -> IO Int

data BotHuman = BotHuman 

instance BotIO BotHuman where
    genmoveIO b g = do
        stToIO (showGame g) >>= putStrLn
        putStr "j ? "
        hFlush stdout
        line <- getLine
        let mK = do j <- readMaybe line
                    U.elemIndex j (_moves g)
        case mK of
            Just k -> return k
            Nothing -> genmoveIO b g

instance BotIO (BotRandom RealWorld) where
    genmoveIO b g = stToIO (genmove b g)

instance BotIO (BotMc RealWorld) where
    genmoveIO b g = stToIO (genmove b g)

instance BotIO (BotMcts RealWorld) where
    genmoveIO b g = stToIO (genmove b g)

instance BotIO (BotNegamax RealWorld) where
    genmoveIO b g = stToIO (genmove b g)

instance BotIO (BotNegamaxAB RealWorld) where
    genmoveIO b g = stToIO (genmove b g)

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

run :: (BotIO b1, BotIO b2) => b1 -> b2 -> Game RealWorld -> IO ()
run botR botY g0
    | isRunning g0 = do
        k <- if _currentPlayer g0 == PlayerR then genmoveIO botR g0
                                             else genmoveIO botY g0
        stToIO (playK k g0) >>= run botR botY 
    |otherwise = do
        stToIO (showGame g0) >>= putStrLn
        putStrLn "new game (y/n) ? ";
        r <- getLine
        when (r == "y") $ stToIO (nextGame g0) >>= run botR botY

main :: IO ()
main = do
    game <- stToIO (mkGame PlayerR) 
    args <- getArgs
    gen <- createSystemRandom
    case splitOn "-" (unwords args) of
        ["random"] -> run BotHuman (BotRandom gen) game
        ["mc", nsimsStr] -> case readMaybe nsimsStr of
            Nothing -> usage
            Just nsims -> run BotHuman (BotMc nsims gen) game
        ["mcts", nsimsStr] -> case readMaybe nsimsStr of
            Nothing -> usage
            Just nsims -> run BotHuman (BotMcts nsims gen) game
        ["negamax", depthStr] -> case readMaybe depthStr of
            Nothing -> usage
            Just depth -> run BotHuman (BotNegamax evalFunc1 depth :: BotNegamax RealWorld) game
        ["negamaxab", depthStr] -> case readMaybe depthStr of
            Nothing -> usage
            Just depth -> run BotHuman (BotNegamaxAB evalFunc1 depth :: BotNegamaxAB RealWorld) game
        _ -> usage

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " <> progName <> " bot"
    putStrLn ""
    putStrLn "bots: "
    putStrLn "  random"
    putStrLn "  mc-<nsims>"
    putStrLn "  mcts-<nsims>"
    putStrLn "  negamax-<nsims>"
    putStrLn "  negamaxab-<nsims>"

