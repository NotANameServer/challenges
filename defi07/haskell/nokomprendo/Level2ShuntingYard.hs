-------------------------------------------------------------------------------
-- Calculatrice infix avec token + Shunting-Yard.
-- 
-- avec ou sans espaces : 
--   12 * (7 - 4)
--   12*(7-4)
-------------------------------------------------------------------------------

module Level2ShuntingYard where

import Control.Monad
import Data.Char
import Data.List
import System.IO

-------------------------------------------------------------------------------
-- operateurs
-------------------------------------------------------------------------------

data Infix
    = InfixR
    | InfixL
    deriving (Eq, Show)

data Op = Op
    { _symbol   :: Char
    , _priority :: Int
    , _infix    :: Infix
    , _function :: [Token] -> Maybe ([Token], Double)
    } 

instance Show Op where
    show (Op s _ _ _) = "Op " ++ [s]

instance Eq Op where
    (Op s1 _ _ _) == (Op s2 _ _ _) = s1 == s2

ops :: [Op]
ops =
    [ Op '+' 6 InfixL (binary (+))
    , Op '-' 6 InfixL (binary (-))
    , Op '*' 7 InfixL (binary (*))
    , Op '/' 7 InfixL (binary (/))
    , Op '^' 8 InfixR (binary (**))
    , Op '!' 9 InfixR (unary fact)
    ]

unary :: (Double -> a) -> [Token] -> Maybe ([Token], a)
unary f xs = do
    (es, e) <- run xs
    return (es, f e)

binary :: (Double -> Double -> a) -> [Token] -> Maybe ([Token], a)
binary op xs = do
    (esR, eR) <- run xs
    (esL, eL) <- run esR
    return (esL, eL `op` eR)

fact :: Double -> Double
fact n = fromIntegral $ product [1 .. (truncate n :: Integer)]

-------------------------------------------------------------------------------
-- tokens
-------------------------------------------------------------------------------

data Token 
    = TokOp Op 
    | TokLeftPar 
    | TokRightPar 
    | TokDot
    | TokDigits String
    | TokNum Double
    deriving (Eq, Show)

isOp :: Token -> Bool
isOp (TokOp _) = True
isOp _ = False

tokenize :: String -> [Token]
tokenize [] = []
tokenize (' ':xs) = tokenize xs
tokenize ('(':xs) = TokLeftPar  : tokenize xs
tokenize (')':xs) = TokRightPar : tokenize xs
tokenize ('.':xs) = TokDot : tokenize xs
tokenize xxs@(x:xs) = 
    case find ((==x) . _symbol) ops of
        Just op -> TokOp op : tokenize xs
        Nothing -> 
            let (digits, ys) = span isDigit xxs
            in if null digits then [] else TokDigits digits : tokenize ys

-------------------------------------------------------------------------------
-- conversion digits vers num (entiers, réels, négatifs)
-------------------------------------------------------------------------------

numberize :: [Token] -> [Token]
numberize l = case l of
    (TokOp (Op '-' _ _ _) : TokDigits x : xs) -> go (TokDigits ('-':x) : xs)
    _ -> go l
    where
        go (TokOp x : TokOp (Op '-' _ _ _) : TokDigits y : xs) = TokOp x : go (TokDigits ('-':y) : xs)
        go (TokDigits x : TokDot : TokDigits y : xs) = TokNum (read $ x++"."++y) : go xs
        go (TokDigits x : xs) = TokNum (read x) : go xs
        go (x:xs) = x : go xs
        go [] = []

-------------------------------------------------------------------------------
-- conversion infix vers postfix
-------------------------------------------------------------------------------

shuntingYard :: [Token] -> Maybe [Token]
shuntingYard tokens = 
    let 
        unstack stack = 
            if all isOp stack then Just stack else Nothing

        handleOp :: Op -> [Token] -> ([Token], [Token])
        handleOp o1 (TokOp o2:stack) =
            if _infix o1 == InfixL && _priority o1 <= _priority o2 || 
               _infix o1 == InfixR && _priority o1 <  _priority o2
            then let (out, stack') = handleOp o1 stack
                 in (out++[TokOp o2], stack')
            else ([], TokOp o1:TokOp o2:stack)
        handleOp o1 stack = ([], TokOp o1:stack)

        go stack [] = unstack stack
        go stack (TokNum v : xs) = (TokNum v :) <$> go stack xs
        go stack (TokOp o1:xs) = 
            let (out, stack') = handleOp o1 stack
            in (out ++) <$> go stack' xs
        go stack (TokLeftPar:xs) = go (TokLeftPar:stack) xs
        go stack (TokRightPar:xs) = 
            case break (==TokLeftPar) stack of
                (o, TokLeftPar:s) -> (o ++) <$> go s xs
                _ -> Nothing
        go _ _ = Nothing

    in go [] tokens

-------------------------------------------------------------------------------
-- calculatrice
-------------------------------------------------------------------------------

run :: [Token] -> Maybe ([Token], Double)
run [] = Nothing
run xs = 
    case (init xs, last xs) of
        (ys, TokOp op) -> _function op ys
        (ys, TokNum v) -> Just (ys, v)
        _ -> Nothing

main :: IO ()
main = do
    putStr "\n> "
    hFlush stdout
    input <- getLine
    when (input /= []) $ do
        print $ numberize $ tokenize input
        print $ shuntingYard $ numberize $ tokenize input
        print $ fmap run $ shuntingYard $ numberize $ tokenize input
        main

