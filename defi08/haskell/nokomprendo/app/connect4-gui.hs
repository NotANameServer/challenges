{-# LANGUAGE FlexibleInstances #-}

import Connect4.Game
import Connect4.Bot
import Connect4.BotNegamax

import Control.Monad.ST
import Data.List.Split
import Data.Massiv.Array (freezeS, Ix2(..), ifoldMono)
import qualified Data.Vector.Unboxed as U
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import System.Exit
import System.Random.MWC
import Text.Read

----------------------------------------------------------------------
-- params
----------------------------------------------------------------------

gCellSize, gHeaderHeight, gWidth, gHeight :: Int
gCellSize = 50
gHeaderHeight = 50
gWidth = gCellSize * nJ
gHeight = gCellSize * nI + gHeaderHeight

gHeightF05, gWidthF05, gHeaderHeightF05, gCellSizeF, gCellSizeF05 :: Float
gHeightF05 = 0.5 * fromIntegral gHeight
gWidthF05 = 0.5 * fromIntegral gWidth
gHeaderHeightF05 = 0.5 * fromIntegral gHeaderHeight
gCellSizeF = fromIntegral gCellSize
gCellSizeF05 = 0.5 * gCellSizeF

----------------------------------------------------------------------
-- bots
----------------------------------------------------------------------

data BotGui
    = BotGuiHuman
    | BotGuiRandom (BotRandom RealWorld)
    | BotGuiMc (BotMc RealWorld)
    | BotGuiMcts (BotMcts RealWorld)
    | BotGuiNegamax (BotNegamax RealWorld)
    | BotGuiNegamaxAB (BotNegamaxAB RealWorld)

-- does the bot need user input ?
botGuiInput :: BotGui -> Bool
botGuiInput BotGuiHuman = True
botGuiInput _ = False

botGuiGenmove :: BotGui -> Game RealWorld -> Int -> IO (Maybe Int)
botGuiGenmove BotGuiHuman g j = return $ U.elemIndex j (_moves g)
botGuiGenmove (BotGuiRandom b) g _ = stToIO $ Just <$> genmove b g
botGuiGenmove (BotGuiMc b) g _ = stToIO $ Just <$> genmove b g
botGuiGenmove (BotGuiMcts b) g _ = stToIO $ Just <$> genmove b g
botGuiGenmove (BotGuiNegamax b) g _ = stToIO $ Just <$> genmove b g
botGuiGenmove (BotGuiNegamaxAB b) g _ = stToIO $ Just <$> genmove b g

mkBotGui :: String -> IO (Maybe BotGui)
mkBotGui "human" = return $ Just BotGuiHuman
mkBotGui "random" = Just . BotGuiRandom . BotRandom <$> createSystemRandom
mkBotGui args = case splitOn "-" args of
    ["mc",str] -> (\n -> BotGuiMc . flip BotMc n <$> readMaybe str) <$> createSystemRandom 
    ["mcts",str] -> (\n -> BotGuiMcts . flip BotMcts n <$> readMaybe str) <$> createSystemRandom 
    ["negamax",str] -> return (BotGuiNegamax . BotNegamax evalFunc1 <$> readMaybe str)
    ["negamaxab",str] -> return (BotGuiNegamaxAB . BotNegamaxAB evalFunc1 <$> readMaybe str)
    _ -> return Nothing

----------------------------------------------------------------------
-- main
----------------------------------------------------------------------

data Model = Model
    { _botR :: BotGui
    , _botY :: BotGui
    , _game :: Game RealWorld
    }

main :: IO ()
main = do
    args <- getArgs
    bots <- mapM mkBotGui args
    case bots of
        [Just botR, Just botY] -> do
            game <- stToIO (mkGame PlayerR)
            let model = Model botR botY game 
            let win = InWindow "Connect4" (gWidth, gHeight) (0, 0)
                bgcolor = makeColor 0.2 0.5 0.2 1.0
            playIO win bgcolor 30 model hDraw hEvent hTime
        _ -> usage

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "usage: " <> progName <> " botR botY"
    putStrLn ""
    putStrLn "bots: "
    putStrLn "  human"
    putStrLn "  random"
    putStrLn "  mc-<nsims>"
    putStrLn "  mcts-<nsims>"
    putStrLn "  negamax-<depth>"
    putStrLn "  negamaxab-<depth>"

----------------------------------------------------------------------
-- GUI handlers
----------------------------------------------------------------------

hEvent :: Event -> Model -> IO Model
hEvent (EventKey (SpecialKey KeyEsc) Up _ _) _ = exitSuccess
hEvent (EventKey (MouseButton RightButton) Up _ _) m = do
    g1 <- stToIO $ nextGame (_game m)
    return m { _game = g1 }
hEvent (EventKey (MouseButton LeftButton) Up _ (x,_)) m@(Model botR botY g)
    | _status g == PlayR = Model botR botY <$> runAction g botR True j
    | _status g == PlayY = Model botR botY <$> runAction g botY True j
    | otherwise = return m
    where j = truncate $ (x+gWidthF05) / gCellSizeF
hEvent _ m = return m

hTime :: Float -> Model -> IO Model
hTime _ m@(Model botR botY g) 
    | _status g == PlayR = Model botR botY <$> runAction g botR False 0
    | _status g == PlayY = Model botR botY <$> runAction g botY False 0
    | otherwise = return m

runAction :: Game RealWorld -> BotGui -> Bool -> Int -> IO (Game RealWorld)
runAction g b ni j0 = do
    if isRunning g && botGuiInput b == ni
    then do
        mk <- botGuiGenmove b g j0
        case mk of
            Nothing -> putStrLn "invalid move" >> return g
            Just k -> stToIO $ playK k g
    else return g

hDraw :: Model -> IO Picture
hDraw m = do
    gamePic <- drawGame m
    return $ Pictures [gamePic, drawGrid, drawStatus m]

drawGrid :: Picture
drawGrid = Pictures 
    [ translate (i*gCellSizeF - gWidthF05) (-gHeaderHeightF05) 
        (Color black $ rectangleSolid 3 (fromIntegral $ gCellSize * nI))
    | i <- [1 .. fromIntegral (nJ-1)] ]

drawStatus :: Model -> Picture
drawStatus m = 
    let bgcolor = if isRunning (_game m) then white else greyN 0.8
    in Pictures
        [ translate 0 (gHeightF05-gHeaderHeightF05) $ Color bgcolor 
            $ rectangleSolid (fromIntegral gWidth) (fromIntegral gHeaderHeight)
        , translate (10 - gWidthF05) (gHeightF05 - 10 - gHeaderHeightF05)
            $ scale 0.2 0.2 (Text $ show $ _status $ _game m)
        ]

drawGame :: Model -> IO Picture
drawGame m = 
    let getColor CellR = red 
        getColor _ = yellow 

        fAcc _ CellE = []
        fAcc (Ix2 i j) c = 
            [ translate 
                (fromIntegral (j*gCellSize) - gWidthF05 + gCellSizeF05) 
                (fromIntegral (i*gCellSize) - gHeightF05 + gCellSizeF05)
                $ Color (getColor c) $ circleSolid (gCellSizeF05 - 2) ]

    in Pictures . ifoldMono fAcc <$> freezeS (_cells $ _game m)


-- gloss coordinate system:
-- +------------+
-- |            |
-- |     ^      |
-- |     |      |
-- |     +-->   |
-- |            |
-- |            |
-- +------------+

