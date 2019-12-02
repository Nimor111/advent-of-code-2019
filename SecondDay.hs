module SecondDay where

import           Data.List       (elemIndex)
import           Data.List.Split (splitOn)

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

main :: IO ()
main = do
    contents <- readFile "SecondDayInput"
    let xs = parseInput contents
        ys = [head xs] ++ [12, 2] ++ drop 3 xs in

        print $ head $ solve ys
