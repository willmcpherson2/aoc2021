module Day6Part2 (main) where
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.List.Split (splitOn)

type Fish = Int

main :: IO ()
main = do
  text <- readFile "input/day6.txt"
  let
    fishes = parse text
    growth = sum $ parMap rdeepseq (spawns (256 + 1)) fishes
  print growth

spawns :: Int -> Fish -> Int
spawns time fish = 1 + childSpawns time fish

childSpawns :: Int -> Fish -> Int
childSpawns time fish =
  let time' = time - (fish + 1)
  in if time' <= 0 then 0 else spawns time' 8 + childSpawns time' 6

parse :: String -> [Fish]
parse = map read . splitOn ","
