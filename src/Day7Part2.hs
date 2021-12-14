module Day7Part2 (main) where
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Position = Int

type Fuel = Int

main :: IO ()
main = do
  text <- readFile "input/day7.txt"
  let
    positions = parse text
    position = solve positions
    fuel = getFuel position positions
  print fuel

getFuel :: Position -> [Position] -> Fuel
getFuel target = sum . map (triangle . distance target)

triangle :: Fuel -> Fuel
triangle fuel = (fuel * (fuel + 1)) `div` 2

distance :: Position -> Position -> Position
distance a b = abs $ a - b

solve :: [Position] -> Position
solve positions =
  let
    min = minimum positions
    max = maximum positions
    best = fst $ fromJust $ foldl (better positions) Nothing [min .. max]
  in best

better :: [Position] -> Maybe (Position, Fuel) -> Int -> Maybe (Position, Fuel)
better positions best position = Just $ case best of
  Just (bestPosition, bestFuel) ->
    let fuel = getFuel position positions
    in if fuel < bestFuel then (position, fuel) else (bestPosition, bestFuel)
  Nothing -> (position, getFuel position positions)

parse :: String -> [Position]
parse = map read . splitOn ","
