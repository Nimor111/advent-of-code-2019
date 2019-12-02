module SecondDay where

import           Data.List       (elemIndex)
import           Data.List.Split (splitOn)
import System.Random (randomRIO)

solve :: [Int] -> [Int]
solve xs = go xs 0
    where
        go :: [Int] -> Int -> [Int]
        go ls idx
             | first == 99 = ls
             | first == 1 =
                 go (left ++ [second + third] ++ right) (idx + 4)
             | first == 2 =
                 go (left ++ [second * third] ++ right) (idx + 4)
             where
                left = take (ls !! (idx + 3)) ls
                first = ls !! idx
                right = drop ((ls !! (idx + 3)) + 1) ls
                second = ls !! (ls !! (idx + 1))
                third = ls !! (ls !! (idx + 2))
                fourth = ls !! (ls !! (idx + 3))

parseInput :: String -> [Int]
parseInput = fmap (\x -> read x :: Int) . splitOn "," . filter (/= '\n')

checkOutput :: [Int] -> IO Int
checkOutput xs = do
    pair <- genPair
    if (head (solved pair)) == 19690720
       then pure $ 100 * (fst pair) + (snd pair)
       else checkOutput xs
    where
        solved pair = solve $ [head xs] ++ [fst pair, snd pair] ++ drop 3 xs

genPair :: IO (Int, Int)
genPair = do
    x <- randomRIO (1, 100)
    y <- randomRIO (1, 100)
    pure (x, y)

main :: IO ()
main = do
    contents <- readFile "SecondDayInput"
    let xs = parseInput contents
        ys = [head xs] ++ [12, 2] ++ drop 3 xs in

        print $ head $ solve ys
