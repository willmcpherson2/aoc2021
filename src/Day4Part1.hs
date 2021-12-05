module Day4Part1 (main) where
import Data.Foldable (find)
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)

newtype Board = Board [[Spot]]
  deriving Show

data Spot = Spot
  { getValue :: Int
  , getMarked :: Bool
  }
  deriving Show

main :: IO ()
main = do
  text <- readFile "input/day4.txt"
  let
    (values, boards) = parse text
    Just (winningValue, winningBoard) = runGame boards values
    score = countScore winningValue winningBoard
  print score

countScore :: Int -> Board -> Int
countScore value (Board spots) =
  let
    unmarkedSum =
      sum $ map (sum . map getValue . filter (not . getMarked)) spots
  in value * unmarkedSum

runGame :: [Board] -> [Int] -> Maybe (Int, Board)
runGame boards values = case values of
  (value : values) ->
    let
      boards' = markBoards value boards
      winningBoard = find boardWin boards'
    in case winningBoard of
      Just board -> Just (value, board)
      Nothing -> runGame boards' values
  _ -> Nothing

boardWin :: Board -> Bool
boardWin (Board spots) =
  let
    rowWin = any (all getMarked) spots
    columnWin = any (all getMarked) (transpose spots)
  in rowWin || columnWin

markBoards :: Int -> [Board] -> [Board]
markBoards value = map (markBoard value)

markBoard :: Int -> Board -> Board
markBoard value (Board spots) = Board $ map (map (markSpot value)) spots

markSpot :: Int -> Spot -> Spot
markSpot value' (Spot value marked) = Spot value (marked || value == value')

parse :: String -> ([Int], [Board])
parse text =
  let
    (valuesSection : boardsSection) = words text
    values = map read (splitOn "," valuesSection)
    boardValues = chunksOf 5 $ chunksOf 5 $ map read boardsSection
    boards = map mkBoard boardValues
  in (values, boards)

mkBoard :: [[Int]] -> Board
mkBoard = Board . map (map mkSpot)

mkSpot :: Int -> Spot
mkSpot value = Spot value False
