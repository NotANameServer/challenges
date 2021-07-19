import Connect4.Bot
import Connect4.BotNegamax
import Connect4.Game

import Control.Monad.ST
import Data.List.Split
import System.Environment
import System.Random.MWC
import System.TimeIt
import Text.Read

-------------------------------------------------------------------------------
-- BotCmp
-------------------------------------------------------------------------------

data BotCmp
    = BotCmpRandom (BotRandom RealWorld)
    | BotCmpMc (BotMc RealWorld)
    | BotCmpMcts (BotMcts RealWorld)
    | BotCmpNegamax (BotNegamax RealWorld)
    | BotCmpNegamaxAB (BotNegamaxAB RealWorld)

mkBotCmp :: String -> IO (Maybe BotCmp)
mkBotCmp "random" = Just . BotCmpRandom . BotRandom <$> createSystemRandom
mkBotCmp args = case splitOn "-" args of
    ["mc",str] -> (\n -> BotCmpMc . flip BotMc n <$> readMaybe str) <$> createSystemRandom 
    ["mcts",str] -> (\n -> BotCmpMcts . flip BotMcts n <$> readMaybe str) <$> createSystemRandom 
    ["negamax",str] -> return (BotCmpNegamax . BotNegamax evalFunc1 <$> readMaybe str)
    ["negamaxab",str] -> return (BotCmpNegamaxAB . BotNegamaxAB evalFunc1 <$> readMaybe str)
    _ -> return Nothing

genmoveCmp :: BotCmp -> Game RealWorld -> IO Int
genmoveCmp (BotCmpRandom b) g = stToIO $ genmove b g
genmoveCmp (BotCmpMc b) g = stToIO $ genmove b g
genmoveCmp (BotCmpMcts b) g = stToIO $ genmove b g
genmoveCmp (BotCmpNegamax b) g = stToIO $ genmove b g
genmoveCmp (BotCmpNegamaxAB b) g = stToIO $ genmove b g

-------------------------------------------------------------------------------
-- cmp
-------------------------------------------------------------------------------

playoutBotsCmp :: BotCmp -> BotCmp -> Game RealWorld -> IO Status
playoutBotsCmp botR botY g0 
    | isRunning g0 =
        let moveFunc = if _currentPlayer g0 == PlayerR then genmoveCmp botR
                                                       else genmoveCmp botY 
        in moveFunc g0 >>= stToIO . (`playK` g0) >>= playoutBotsCmp botR botY 
    | otherwise = return (_status g0) 

run :: BotCmp -> BotCmp -> Int -> IO (Double, Double, Double)
run botR botY nGames = 
    let aux 0 r y t _g0 = 
            let nGamesD = fromIntegral nGames
                rD = fromIntegral r
                yD = fromIntegral y
                tD = fromIntegral t
            in return (rD/nGamesD, yD/nGamesD, tD/nGamesD)
        aux n r y t g0 = do
            g1 <- stToIO $ nextGame g0
            s1 <- playoutBotsCmp botR botY g1
            case s1 of
                WinR -> aux (n-1) (r+1) y t g1
                WinY -> aux (n-1) r (y+1) t g1
                Tie  -> aux (n-1) r y (t+1) g1
                _ -> error "game not terminated"
    in stToIO (mkGame PlayerR) >>= aux nGames (0::Int) (0::Int) (0::Int)

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    case args of
        (nGamesStr:argsBot) -> do
            bots <- mapM mkBotCmp argsBot
            case (readMaybe nGamesStr, bots) of
                (Just nGames, [Just botR, Just botY]) -> do
                    putStrLn "winR WinY tie ry ryt dt nGames botR botY"
                    (dt, (r, y, t)) <- timeItT $ run botR botY nGames
                    putStrLn $ unwords (map show [r, y, t, r+y, r+y+t, dt] ++ args)
                _ -> usage
        _ -> usage

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " <> progName <> " nGames botR botY"
    putStrLn ""
    putStrLn "bots: "
    putStrLn "  random"
    putStrLn "  mc-<nsims>"
    putStrLn "  mcts-<nsims>"
    putStrLn "  negamax-<depth>"
    putStrLn "  negamaxab-<depth>"

