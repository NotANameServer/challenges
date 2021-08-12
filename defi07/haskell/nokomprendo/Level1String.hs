-------------------------------------------------------------------------------
-- Calculatrice postfix, sans token.
-------------------------------------------------------------------------------

module Level1String where

import Control.Monad
import System.IO
import Text.Read

-------------------------------------------------------------------------------
-- calculatrice
-------------------------------------------------------------------------------

run :: [String] -> Maybe ([String], Double)
run [] = Nothing
run xs = 
    case (init xs, last xs) of
        (ys, "+") -> binary (+) ys
        (ys, "-") -> binary (-) ys
        (ys, "*") -> binary (*) ys
        (ys, "/") -> binary (/) ys
        (ys, "^") -> binary (**) ys
        (ys, "!") -> fmap (fmap fact) (run ys)
        (ys, y) -> 
            case readMaybe y of
                Nothing -> Nothing
                Just v -> Just (ys, v)

binary :: (Double -> Double -> a) -> [String] -> Maybe ([String], a)
binary op xs = do
    (esR, eR) <- run xs
    (esL, eL) <- run esR
    return (esL, eL `op` eR)

fact :: Double -> Double
fact n = fromIntegral $ product [1 .. (truncate n :: Integer)]

main :: IO ()
main = do
    putStr "\n> "
    hFlush stdout
    input <- words <$> getLine
    when (input /= []) $ do
        print input
        print $ run input
        main

