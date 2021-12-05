module Day3Part2 (main) where

data Digit = Zero | One
  deriving (Show, Eq)

type BinaryInt = [Digit]

type Count = [Int]

type CountFunction = [BinaryInt] -> BinaryInt

main :: IO ()
main = do
  input <- readFile "input/day3.txt"
  let
    ints = map (map parseDigit) (lines input)
    oxygen = filterAll getGamma ints
    co2 = filterAll getEpsilon ints
    oxygenDecimal = binaryIntToDecimal (head oxygen)
    co2Decimal = binaryIntToDecimal (head co2)
    life = oxygenDecimal * co2Decimal
  print life

filterAll :: CountFunction -> [BinaryInt] -> [BinaryInt]
filterAll f rows = foldl (filterOrFinish f) rows [0 .. 12]

filterOrFinish :: CountFunction -> [BinaryInt] -> Int -> [BinaryInt]
filterOrFinish f rows column = case rows of
  [result] -> [result]
  _ ->
    let filtered = filterRows (f rows) column rows
    in if null filtered then [last rows] else filtered

filterRows :: BinaryInt -> Int -> [BinaryInt] -> [BinaryInt]
filterRows matcher column = filter (columnMatch matcher column)

columnMatch :: BinaryInt -> Int -> BinaryInt -> Bool
columnMatch matcher column row = row !! column == matcher !! column

getGamma :: CountFunction
getGamma = mostCommon . count

getEpsilon :: CountFunction
getEpsilon = leastCommon . count

mostCommon :: Count -> [Digit]
mostCommon = map (\x -> if x >= 0 then One else Zero)

leastCommon :: Count -> [Digit]
leastCommon = map (\x -> if x >= 0 then Zero else One)

count :: [BinaryInt] -> Count
count = foldr countRow mkCount

countRow :: BinaryInt -> Count -> Count
countRow = zipWith countDigit

countDigit :: Digit -> Int -> Int
countDigit digit int = case digit of
  Zero -> int - 1
  One -> int + 1

mkCount :: Count
mkCount = replicate 12 0

binaryIntToDecimal :: BinaryInt -> Int
binaryIntToDecimal int = fst $ foldl
  (\(sum, base) digit -> (sum + digitToDecimal digit * base, base * 2))
  (0, 1)
  (reverse int)

digitToDecimal :: Digit -> Int
digitToDecimal digit = case digit of
  Zero -> 0
  One -> 1

parseDigit :: Char -> Digit
parseDigit ch = case ch of
  '0' -> Zero
  '1' -> One
  _ -> undefined
