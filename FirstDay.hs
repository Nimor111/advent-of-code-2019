module FirstDay where

getFuel :: Int -> Int
getFuel = (\x -> x - 2) . floor . (/ 3) . fromIntegral

getFuelReqs :: String -> Int
getFuelReqs contents = go (lines contents)
    where
        go :: [String] -> Int
        go cs = sum $ fmap getFuel (nums cs)

        nums :: [String] -> [Int]
        nums cs = fmap (\x -> read x :: Int) cs

getFuelReqsTillZero :: Int -> Int
getFuelReqsTillZero x
    | fuel <= 0 = 0
    | otherwise = fuel + getFuelReqsTillZero fuel
    where
        fuel :: Int
        fuel = getFuel x

getAllFuelReqs :: String -> Int
getAllFuelReqs contents = go (lines contents)
    where
        go :: [String] -> Int
        go cs = sum $ fmap getFuelReqsTillZero (nums cs)

        nums :: [String] -> [Int]
        nums cs = fmap (\x -> read x :: Int) cs

main :: IO ()
main = do
    contents <- readFile "FirstDayInput"
    print $ getFuelReqs contents
    print $ getAllFuelReqs contents
