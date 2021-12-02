module Day1Part2 (main) where

main :: IO ()
main = do
  input <- readFile "input/day1.txt"
  let
    measurements = map read (lines input)
    windows = getWindows measurements
    sums = getSums windows
    increases = countIncreases sums
  print increases

type Measurement = Int

type Increases = Int

type Window = (Measurement, Measurement, Measurement)

countIncreases :: [Measurement] -> Int
countIncreases = fst . foldl count (0, Nothing)

count
  :: (Increases, Maybe Measurement)
  -> Measurement
  -> (Increases, Maybe Measurement)
count (increases, prevMeasurement) x = case prevMeasurement of
  Nothing -> (increases, Just x)
  Just prevMeasurement ->
    (if x > prevMeasurement then increases + 1 else increases, Just x)

getSums :: [Window] -> [Measurement]
getSums = map (\(x, y, z) -> x + y + z)

getWindows :: [Measurement] -> [Window]
getWindows xs = case xs of
  x : y : z : xs -> (x, y, z) : getWindows (y : z : xs)
  _ -> []
