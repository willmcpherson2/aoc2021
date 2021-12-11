module Day5Part2 (main) where
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
pointsInLine (a@(x1, y1), b@(x2, y2)) =
  let
    deltaX = getDelta x1 x2
    deltaY = getDelta y1 y2
    points = iterate (\(x, y) -> (x + deltaX, y + deltaY)) a
  in b : takeWhile (/= b) points

getDelta :: Int -> Int -> Int
getDelta a b = case compare a b of
  LT -> 1
  EQ -> 0
  GT -> -1

parseLine :: String -> Line
parseLine text =
  let [a, b] = splitOn "->" text in (parsePoint a, parsePoint b)

parsePoint :: String -> Point
parsePoint text = let [x, y] = splitOn "," text in (read x, read y)
