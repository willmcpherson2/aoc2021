module Day2Part2 (main) where

data Command = Command Direction Int

data Direction = Up | Down | Forward

type State = (Int, Int, Int)

main :: IO ()
main = do
  input <- readFile "input/day2.txt"
  let
    commandStrings = map words (lines input)
    commands = map parseCommand commandStrings
    (x, y, _) = foldl count (0, 0, 0) commands
    total = x * y
  print total

count :: State -> Command -> State
count (x, y, aim) command = case command of
  Command Up amount -> (x, y, aim - amount)
  Command Down amount -> (x, y, aim + amount)
  Command Forward amount -> (x + amount, y + aim * amount, aim)

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
