module Day3Part1 (main) where

data Digit = Zero | One

type BinaryInt = [Digit]

type Count = [Int]

main :: IO ()
main = do
  input <- readFile "input/day3.txt"
  let
    digits = map (map parseDigit) (lines input)
    gamma = getGamma digits
    epsilon = getEpsilon digits
    gammaDecimal = binaryIntToDecimal gamma
    epsilonDecimal = binaryIntToDecimal epsilon
    powerConsumption = gammaDecimal * epsilonDecimal
  print powerConsumption

getGamma :: [BinaryInt] -> BinaryInt
getGamma = mostCommon . count

getEpsilon :: [BinaryInt] -> BinaryInt
getEpsilon = leastCommon . count

mostCommon :: Count -> [Digit]
mostCommon = map (\x -> if x > 0 then One else Zero)

leastCommon :: Count -> [Digit]
leastCommon = map (\x -> if x > 0 then Zero else One)

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
