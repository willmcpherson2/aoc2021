module Day1 (main) where

main :: IO ()
main = do
  input <- readFile "input/day1.txt"
  let
    measurements = map read (lines input)
    increases = countIncreases measurements
  print increases

type Measurement = Int

type Increases = Int

countIncreases :: [Measurement] -> Increases
countIncreases = fst . foldl count (0, Nothing)

count
  :: (Increases, Maybe Measurement)
  -> Measurement
  -> (Increases, Maybe Measurement)
count (increases, prevMeasurement) x = case prevMeasurement of
  Nothing -> (increases, Just x)
  Just prevMeasurement ->
    (if x > prevMeasurement then increases + 1 else increases, Just x)
