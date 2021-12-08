{-# LANGUAGE TupleSections #-}

module Day5Part1 (main) where
import Data.List (group, sort)
import Data.List.Split (splitOn)

type Line = (Point, Point)

type Point = (Int, Int)

main :: IO ()
main = do
  text <- readFile "input/day5.txt"
  let
    segments = map parseLine (lines text)
    points = concatMap pointsInLine segments
    score = count points
  print score

count :: [Point] -> Int
count points = length $ filter (>= 2) $ map length $ group $ sort points

pointsInLine :: Line -> [Point]
pointsInLine ((x1, y1), (x2, y2))
  | x1 < x2 = fill x1 x2 y1 y2 (, y1)
  | x2 < x1 = fill x2 x1 y1 y2 (, y1)
  | y1 < y2 = fill y1 y2 x1 x2 (x1, )
  | y2 < y1 = fill y2 y1 x1 x2 (x1, )
  | otherwise = undefined
  where fill a b c d f = if c == d then map f [a .. b] else []

parseLine :: String -> Line
parseLine text =
  let [a, b] = splitOn "->" text in (parsePoint a, parsePoint b)

parsePoint :: String -> Point
parsePoint text = let [x, y] = splitOn "," text in (read x, read y)
