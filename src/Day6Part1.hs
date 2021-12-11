module Day6Part1 (main) where
import Data.List.Split (splitOn)

type Fish = Int

main :: IO ()
main = do
  text <- readFile "input/day6.txt"
  let
    fishes = parse text
    growth = length $ simulate 80 fishes
  print growth

simulate :: Int -> [Fish] -> [Fish]
simulate n fishes = case n of
  0 -> fishes
  _ -> simulate (n - 1) (stepAll fishes)

stepAll :: [Fish] -> [Fish]
stepAll = foldr step []
  where
    step fish fishes = case fish of
      0 -> 6 : 8 : fishes
      _ -> fish - 1 : fishes

parse :: String -> [Fish]
parse = map read . splitOn ","
