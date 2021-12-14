module Day7Part1 (main) where
import Data.List (sort)
import Data.List.Split (splitOn)

type Position = Int

type Fuel = Int

main :: IO ()
main = do
  text <- readFile "input/day7.txt"
  let
    positions = parse text
    position = median positions
    fuel = getFuel position positions
  print fuel

getFuel :: Position -> [Position] -> Fuel
getFuel target = sum . map (distance target)

distance :: Position -> Position -> Position
distance a b = abs $ a - b

median :: [Position] -> Position
median positions =
  let
    sorted = sort positions
    mid = length positions `div` 2
  in sorted !! mid

parse :: String -> [Position]
parse = map read . splitOn ","
