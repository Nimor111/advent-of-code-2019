module ThirdDay where

import Data.Attoparsec.ByteString.Char8 hiding (D)
import Control.Applicative (liftA2, (<|>))
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import Data.List (intersect)
import Data.List.Split (splitOn)

-- Idea for algorithm
-- Mark every point a line has passed through
-- If other line passes through a marked point, add to intersection list
-- Use point lies in line segment -> distance a b = sqrt((a.x - b.x)**2 + (a.y - b.y)**2)
-- and point lies if distance(a, c) + distance(c, b) == distance(a, b)

data Point = Point
    { x :: Int
    , y :: Int
    } deriving (Show, Eq)

type Board = [Point]

data Command = U Int | D Int | L Int | R Int deriving Read

uParser :: Parser Command
uParser = liftA2 (\_ -> U) (char 'U') decimal

dParser :: Parser Command
dParser = liftA2 (\_ -> D) (char 'D') decimal

lParser :: Parser Command
lParser = liftA2 (\_ -> L) (char 'L') decimal

rParser :: Parser Command
rParser = liftA2 (\_ -> R) (char 'R') decimal

commandParser :: Parser Command
commandParser = uParser <|> dParser <|> lParser <|> rParser

instance Show Command where
    show (U i) = "U" ++ show i
    show (D i) = "D" ++ show i
    show (L i) = "L" ++ show i
    show (R i) = "R" ++ show i

manhattanDist :: Point -> Point -> Integer
manhattanDist p1 p2 = fromIntegral $ abs (x p1 - x p2) + abs (y p1 - y p2)

performMoves :: Board -> [Either String Command] -> Board
performMoves b [] = b
performMoves b (x:xs) = case x of
                          Left l -> error "Handle me!"
                          Right r -> performMoves (move b r) xs

repeatN :: Board -> (Point -> Point) -> Int -> Board
repeatN b _ 0 = b
repeatN b f n = repeatN (b ++ [f (last b)]) f (n - 1)

move :: Board -> Command -> Board
move b (U i) = repeatN b (\p -> Point (x p) (y p + 1)) i
move b (D i) = repeatN b (\p -> Point (x p) (y p - 1)) i
move b (R i) = repeatN b (\p -> Point (x p + 1) (y p)) i
move b (L i) = repeatN b (\p -> Point (x p - 1) (y p)) i

parseCommands :: [String] -> [Either String Command]
parseCommands = map (\x -> parseOnly commandParser ((encodeUtf8 . T.pack) x))

main :: IO()
main = do
    c <- readFile "ThirdDayInput"
    let contents = lines c
        firstWire = splitOn "," $ contents !! 0
        secondWire = splitOn "," $ contents !! 1
        firstCommands = parseCommands firstWire
        secondCommands = parseCommands secondWire
        board = [Point 0 0] in
        print $ minimum (map (manhattanDist (Point 0 0)) $ (filter (/= (Point 0 0)) $ intersect (performMoves board firstCommands) (performMoves board secondCommands)))
