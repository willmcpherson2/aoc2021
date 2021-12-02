module Day2Part1 (main) where

data Command = Command Direction Int

data Direction = Up | Down | Forward

main :: IO ()
main = do
  input <- readFile "input/day2.txt"
  let
    commandStrings = map words (lines input)
    commands = map parseCommand commandStrings
    (x, y) = foldl count (0, 0) commands
    total = x * y
  print total

count :: (Int, Int) -> Command -> (Int, Int)
count (x, y) command = case command of
  Command Up amount -> (x, y - amount)
  Command Down amount -> (x, y + amount)
  Command Forward amount -> (x + amount, y)

parseCommand :: [String] -> Command
parseCommand [direction, amount] =
  Command (parseDirection direction) (parseAmount amount)
parseCommand _ = undefined

parseDirection :: String -> Direction
parseDirection direction = case direction of
  "up" -> Up
  "down" -> Down
  "forward" -> Forward
  _ -> undefined

parseAmount :: String -> Int
parseAmount = read
