{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Connect4.Bot where

import Connect4.Game

import qualified Data.Vector.Mutable as MV

import Control.Monad
import Control.Monad.ST
import Data.STRef
import System.Random.MWC

----------------------------------------------------------------------
-- Bot 
----------------------------------------------------------------------

class Bot s b where
    genmove :: b -> Game s -> ST s Int

playoutBots :: (Bot s b1, Bot s b2) => b1 -> b2 -> Game s -> ST s Status
playoutBots botR botY g0 
    | isRunning g0 =
        let moveFunc = if _currentPlayer g0 == PlayerR then genmove botR
                                                       else genmove botY 
        in moveFunc g0 >>= (`playK` g0) >>= playoutBots botR botY 
    | otherwise = return (_status g0) 

----------------------------------------------------------------------
-- BotRandom
----------------------------------------------------------------------

newtype BotRandom s = BotRandom { randomGen :: GenST s }

instance Bot s (BotRandom s) where
    genmove (BotRandom gen) = randomMove gen

{-# INLINE randomMove #-}
randomMove :: GenST s -> Game s -> ST s Int
randomMove gen game = uniformR (0, nMovesGame game - 1) gen

----------------------------------------------------------------------
-- BotMc
----------------------------------------------------------------------

data BotMc s = BotMc
    { mcNsims   :: !Int
    , mcGen     :: !(GenST s)
    }

instance Bot s (BotMc s) where
    genmove (BotMc nsims gen) game =
        let aux ki k s = if ki == nMovesGame game then return k else do
                            si <- evalMove game gen nsims ki
                            if si>s then aux (ki+1) ki si else aux (ki+1) k s
        in aux 0 0 (-1)

evalMove :: Game s -> GenST s -> Int -> Int -> ST s Double
evalMove game0 gen nsims k = do
    let player0 = _currentPlayer game0
    game1 <- cloneGame game0 >>= playK k
    let aux 0 s = return s
        aux i s = do status2 <- cloneGame game1 >>= playoutRandom gen 
                     aux (i - 1) (s + computeScore player0 status2)
    aux nsims 0

playoutRandom :: GenST s -> Game s -> ST s Status
playoutRandom gen g0 
    | isRunning g0 = randomMove gen g0 >>= (`playK` g0) >>= playoutRandom gen
    | otherwise = return (_status g0)

computeScore :: Player -> Status -> Double
computeScore PlayerR WinR = 1.0
computeScore PlayerY WinY = 1.0
computeScore _ Tie = 0.5
computeScore _ _ = 0.0

----------------------------------------------------------------------
-- BotMcts
----------------------------------------------------------------------

mctsKuct :: Double
mctsKuct = 0.5

data BotMcts s = BotMcts
    { mctsNiters    :: !Int
    , mctsGen       :: !(GenST s)
    }

instance Bot s (BotMcts s) where
    genmove (BotMcts niters gen) game = do
        root <- mkRoot game 
        replicateM_ niters $ do
            leaf <- selectAndExpand root
            status <- simulate gen leaf
            backpropagate status leaf
        bestNode root

type NodeRef s = STRef s (Node s)

data Node s = Node 
    { nodeGame      :: !(Game s)
    , nodePlayer    :: !Player     -- before move
    , nodeNmoves    :: !Int
    , nodeParent    :: !(Maybe (NodeRef s))
    , nodeReward    :: !(STRef s Double)
    , nodeNsims     :: !(STRef s Int)
    , nodeLastI     :: !(STRef s Int)
    , nodeChildren  :: !(MV.STVector s (Node s))
    }

mkRoot :: Game s -> ST s (Node s)
mkRoot = mkNode return Nothing

mkLeaf :: Int -> NodeRef s -> ST s (Node s)
mkLeaf k node = do
    game <- nodeGame <$> readSTRef node
    mkNode (playK k) (Just node) game

mkNode :: (Game s -> ST s (Game s)) -> Maybe (NodeRef s) -> Game s -> ST s (Node s)
mkNode gameFunc pNode game0 = do
    game1 <- cloneGame game0 >>= gameFunc
    let nMoves = nMovesGame game1
        player0 = _currentPlayer game0
    Node game1 player0 nMoves pNode 
        <$> newSTRef 0 <*> newSTRef 0 <*> newSTRef 0 <*> MV.new nMoves

bestNode :: Node s -> ST s Int
bestNode root = do
    lastI <- readSTRef (nodeLastI root)
    when (lastI < nodeNmoves root) (error "niters too low")
    let bestNodeFunc (nn,ii) i node = do
            nsims <- readSTRef (nodeNsims node)
            return $ if nsims > nn then (nsims, i) else (nn, ii)
    snd <$> MV.ifoldM' bestNodeFunc (-1, -1) (nodeChildren root)

ucb1 :: Double -> Int -> Int -> Double
ucb1 cReward cNsims pNsims =
    let cNsimsD = fromIntegral cNsims 
        exploitation = cReward / cNsimsD
        exploration = sqrt (log (fromIntegral $ 1 + pNsims) / cNsimsD)
    in exploitation + mctsKuct * exploration

selectUcb :: Node s -> ST s (Node s)
selectUcb node = do
    pNsims <- readSTRef (nodeNsims node)
    let children = nodeChildren node
    let bestUcb1Func (sk, k) i n = do
            reward <- readSTRef (nodeReward n)
            nsims <- readSTRef (nodeNsims n)
            let si = ucb1 reward nsims pNsims 
            return $ if si > sk then (si, i) else (sk, k)
    (_, k) <- MV.ifoldM' bestUcb1Func (-1, -1) children 
    MV.read children k

selectAndExpand :: Node s -> ST s (Node s)
selectAndExpand node = 
    if isRunning (nodeGame node)
    then do
        lastI <- readSTRef (nodeLastI node)
        if lastI < nodeNmoves node
        then do
            nodeRef <- newSTRef node
            cNode <- mkLeaf lastI nodeRef
            MV.write (nodeChildren node) lastI cNode
            modifySTRef' (nodeLastI node) (+1)
            return cNode
        else selectUcb node >>= selectAndExpand 
    else return node 

simulate :: Gen s -> Node s -> ST s Status
simulate gen node = cloneGame (nodeGame node) >>= playoutRandom gen 

backpropagate :: Status -> Node s -> ST s ()
backpropagate status node = do
    modifySTRef' (nodeReward node) (+ computeScore (nodePlayer node) status)
    modifySTRef' (nodeNsims node) (+1)
    forM_ (nodeParent node) (readSTRef >=> backpropagate status)

