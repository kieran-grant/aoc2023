import Data.Char (digitToInt, isAlphaNum, isDigit)
import Data.List (findIndices)
import Data.Maybe (mapMaybe)

data ParsedInt = ParsedInt
    { value :: Int
    , startX :: Int
    , endX :: Int
    , row :: Int
    }
    deriving (Show, Eq)

type Gear = Int

main = do
    contents <- readFile "input.txt"
    let ls = lines contents
    let paddedLs = addPadding ls
    let gearParts = unique . concat $ consume paddedLs
    print $ sum $ map getGearRatio gearParts

consume :: [String] -> [[(ParsedInt, ParsedInt)]]
consume strings = consume' (zip strings [0 ..]) []

consume' :: [(String, Int)] -> [[(ParsedInt, ParsedInt)]] -> [[(ParsedInt, ParsedInt)]]
consume' ((prev, _) : (curr, currY) : (next, nextY) : strings) ints = consume' ((curr, currY) : (next, nextY) : strings) (ints ++ [getGearParts prev curr next currY])
consume' _ ints = ints

getGearRatio :: (ParsedInt, ParsedInt) -> Int
getGearRatio (a, b) = parsedIntToInt a * parsedIntToInt b

getGearParts :: String -> String -> String -> Int -> [(ParsedInt, ParsedInt)]
getGearParts prev curr next currY =
    let x = getNums prev (currY - 1)
        y = getNums curr currY
        z = getNums next (currY + 1)
        gears = getGears curr
        allNums = x ++ y ++ z
     in mapMaybe (getAdjNums' allNums) gears

getAdjNums' :: [ParsedInt] -> Gear -> Maybe (ParsedInt, ParsedInt)
getAdjNums' nums gear =
    if length adjNums == 2
        then Just (head adjNums, last adjNums)
        else Nothing
  where
    adjNums = filter (inRange gear) nums

inRange :: Gear -> ParsedInt -> Bool
inRange sym n = (endX n >= (sym - 1)) && (startX n <= (sym + 1))

getGears :: String -> [Gear]
getGears = findIndices isGear

isGear :: Char -> Bool
isGear x = x == '*'

getNums :: String -> Int -> [ParsedInt]
getNums str y = getNums' str 0 y [] []

-- line -> xPos -> yPos -> numAcc -> numsSoFar -> output
getNums' :: String -> Int -> Int -> String -> [ParsedInt] -> [ParsedInt]
getNums' [] _ _ [] allNums = allNums
getNums' [] x y numAcc allNums = allNums ++ [strToParsedInt numAcc x y]
getNums' (c : cs) x y numAcc allNums
    | isDigit c = getNums' cs (x + 1) y (numAcc ++ [c]) allNums -- current char is digit, add to running number and continue
    | null numAcc = getNums' cs (x + 1) y [] allNums -- current char isn't a digit, and running list is empty, just continue
    | otherwise = getNums' cs (x + 1) y [] (allNums ++ [strToParsedInt numAcc x y]) -- current char isn't a digit, and have accumulated some numbers, turn that into an int

strToParsedInt :: String -> Int -> Int -> ParsedInt
strToParsedInt str x y = ParsedInt{value = strToInt str, startX = x - length str, endX = x - 1, row = y}

strToInt :: [Char] -> Int
strToInt x = read x :: Int

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (x /=) xs)

parsedIntToInt :: ParsedInt -> Int
parsedIntToInt = value

-- padding functions
addPadding :: [String] -> [String]
addPadding toPad = addPadding' toPad []

addPadding' :: [String] -> [String] -> [String]
addPadding' (x : xs) [] = addPadding' (x : xs) [makeRowOfDots $ length x + 2]
addPadding' (x : xs) ys = addPadding' xs (ys ++ [appendDotsToLine x])
addPadding' [] (y : ys) = [y] ++ ys ++ [y]

appendDotsToLine :: String -> String
appendDotsToLine str = '.' : (str ++ ['.'])

makeRowOfDots :: Int -> String
makeRowOfDots n = replicate n '.'
