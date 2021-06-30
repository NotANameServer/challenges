-------------------------------------------------------------------------------
-- Calculatrice postfix, avec token.
-------------------------------------------------------------------------------

module Level1Token where

import Control.Monad
import Data.List
import System.IO

-------------------------------------------------------------------------------
-- operateurs
-------------------------------------------------------------------------------

data Op = Op
    { _symbol   :: String
    , _function :: [Token] -> Maybe ([Token], Double)
    } 

instance Show Op where
    show (Op s _) = "Op " ++ s

instance Eq Op where
    (Op s1 _) == (Op s2 _) = s1 == s2

ops :: [Op]
ops =
    [ Op "+" (binary (+))
    , Op "-" (binary (-))
    , Op "*" (binary (*))
    , Op "/" (binary (/))
    , Op "^" (binary (**))
    , Op "!" (unary fact)
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
    | TokNum Double
    deriving (Eq, Show)

tokenize :: [String] -> [Token]
tokenize [] = []
tokenize (x:xs) = 
    case find ((==x) . _symbol) ops of
        Just op -> TokOp op : tokenize xs
        Nothing -> TokNum (read x) : tokenize xs     -- TODO gestion d'erreurs

-------------------------------------------------------------------------------
-- calculatrice
-------------------------------------------------------------------------------

run :: [Token] -> Maybe ([Token], Double)
run [] = Nothing
run xs = 
    case (init xs, last xs) of
        (ys, TokOp op) -> _function op ys
        (ys, TokNum v) -> Just (ys, v)

main :: IO ()
main = do
    putStr "\n> "
    hFlush stdout
    input <- words <$> getLine
    when (input /= []) $ do
        print  input
        print $ tokenize input
        print $ run $ tokenize input
        main

