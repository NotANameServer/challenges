import System.Random

range :: (Int, Int)
range = (1, 1000000)

nTries :: Int
nTries = 50

mkPropose :: IO (Int -> Int)
mkPropose = do
    target <- randomRIO range
    let cmp x | target < x = -1
              | target > x =  1
              | otherwise  =  0
    return cmp

trouver :: (Int -> Int) -> IO ()
trouver propose = 
    let go 0 _ = putStrLn "perdu"
        go n (a, b) = do
            let m = (b+a) `div` 2
            case propose m of
                -1 -> putStrLn (show m ++ ": trop grand") >> go (n-1) (a, m - 1)
                1  -> putStrLn (show m ++ ": trop petit") >> go (n-1) (m + 1, b)
                _  -> putStrLn (show m ++ ": trouvé") 
    in go nTries range

main :: IO ()
main = mkPropose >>= trouver

