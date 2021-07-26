module Connect4.Game where

import Control.Monad.ST
import qualified Data.Vector.Unboxed as U

import Data.Massiv.Array
import Data.Massiv.Array.Unsafe

nI, nJ :: Int
(nI, nJ) = (6, 7)

data Cell = CellE | CellR | CellY deriving (Eq, Show)

data Player = PlayerR | PlayerY deriving (Eq, Show)

player2Cell :: Player -> Cell
player2Cell PlayerR = CellR
player2Cell PlayerY = CellY

nextPlayer :: Player -> Player
nextPlayer PlayerR = PlayerY
nextPlayer PlayerY = PlayerR

data Status = PlayR | PlayY | Tie | WinR | WinY deriving (Eq, Show)

type Board s = MArray s B Ix2 Cell

data Game s = Game
    { _status           :: !Status
    , _currentPlayer    :: !Player
    , _firstPlayer      :: !Player
    , _moves            :: !(U.Vector Int)
    , _cells            :: !(Board s)
    }

checkIJ :: Int -> Int -> Bool
checkIJ i j = i>=0 && i<nI && j>=0 && j<nJ

lineLength :: Int -> Int -> Int -> Int -> Cell -> Board s -> ST s Int
lineLength i0 j0 di dj c0 cs = 
    let aux i j n = if not (checkIJ i j) then return n else do
                        c <- readM cs (Ix2 i j)
                        if c /= c0 then return n else aux (i+di) (j+dj) (n+1)
    in aux (i0+di) (j0+dj) 0

checkLine :: Int -> Int -> Int -> Int -> Cell -> Board s -> ST s Bool
checkLine i0 j0 di dj c0 cs = do
    l1 <- lineLength i0 j0 di dj c0 cs
    l2 <- lineLength i0 j0 (-di) (-dj) c0 cs
    return $ l1+l2 >= 3

mkGame :: Player -> ST s (Game s)
mkGame p = 
    let status = if p == PlayerR then PlayR else PlayY
    in Game status p p (U.fromList [0 .. nJ-1]) <$> newMArray (Sz2 nI nJ) CellE

cloneGame :: Game s -> ST s (Game s)
cloneGame game0 = do
    b1 <- freezeS (_cells game0) >>= unsafeThaw
    return game0 { _cells = b1 }

nextGame :: Game s -> ST s (Game s)
nextGame g0 = mkGame (nextPlayer $ _firstPlayer g0)

playK :: Int -> Game s -> ST s (Game s)
playK k g@(Game _ _ _ ms _) = playJ (ms U.! k) g

playJ :: Int -> Game s -> ST s (Game s)
playJ j0 g@(Game _ cp _ ms cs) = do

    -- find and play cell
    let findI 0 = return 0
        findI i = do
            c <- readM cs (Ix2 (i-1) j0)
            if c/=CellE then return i else findI (i-1)
    i0 <- findI nI
    let c0 = player2Cell cp
    writeM cs (Ix2 i0 j0) c0
    let ms1 = if i0/=(nI-1) then ms else U.filter (/=j0) ms

    -- update status/current/moves
    resRow <- checkLine i0 j0 0 1 c0 cs
    resCol <- checkLine i0 j0 1 0 c0 cs
    resDiag1 <- checkLine i0 j0 1 1 c0 cs
    resDiag2 <- checkLine i0 j0 1 (-1) c0 cs
    return $ case (resRow||resCol||resDiag1||resDiag2, U.null ms1) of
        (True, _) -> g { _status = if cp==PlayerR then WinR else WinY
                       , _currentPlayer = nextPlayer cp
                       , _moves = U.empty }
        (_, True) -> g { _status = Tie
                       , _currentPlayer = nextPlayer cp
                       , _moves = U.empty }
        _         -> g { _status = if cp==PlayerR then PlayY else PlayR
                       , _currentPlayer = nextPlayer cp
                       , _moves = ms1 }

isRunning :: Game s -> Bool
isRunning (Game status _ _ _ _) = status == PlayR || status == PlayY

nMovesGame :: Game s -> Int
nMovesGame = U.length . _moves

